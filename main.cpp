// macros
#if 1
#  define ndebug
#else
#  define debug
#endif

//#define trace


// c includes

#include <cassert> // needs NDEBUG
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <ctime>

// C++ includes

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cstring>
#include <iterator>

// C++11 includes

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

// macros

#ifdef _MSC_VER
#  define I64(n) (n##i64)
#  define U64(u) (u##ui64)
#else
#  define I64(n) (n##LL)
#  define U64(u) (u##ULL)
#endif

// types

typedef signed char int8;
typedef unsigned char uint8;

typedef signed short int int16;
typedef unsigned short int uint16;

typedef signed int int32;
typedef unsigned int uint32;

typedef signed long long int int64;
typedef unsigned long long int uint64;

typedef uint64 bit_t;
typedef uint64 hash_t; // key_t is used by Unix :(

namespace util {

  class Timer {

    private:

      typedef std::chrono::time_point<std::chrono::system_clock> time_t;
      typedef std::chrono::duration<int, std::ratio<1, 1000>> millisecond_t;

      int elapsed_;
      bool running_;
      time_t start_;

      static time_t now() {
        return std::chrono::system_clock::now();
      }

      int time() const {
        assert(running_);
        return std::chrono::duration_cast<millisecond_t>(now() - start_).count();
      }

    public:

      Timer() {
        reset();
      }

      void reset() {
        elapsed_ = 0;
        running_ = false;
      }

      void start() {
        start_ = now();
        running_ = true;
      }

      void stop() {
        elapsed_ += time();
        running_ = false;
      }

      int elapsed() const {
        int time = elapsed_;
        if (running_)
          time += this->time();
        return time;
      }

  };

  class Lockable {

    protected:
      // HACK for Waitable::wait()

      mutable std::mutex mutex_;

    public:

      void lock() const {
        mutex_.lock();
      }
      void unlock() const {
        mutex_.unlock();
      }
  };

  class Waitable: public Lockable {

    private:

      std::condition_variable_any p_cond;

    public:

      void wait() {
        p_cond.wait(mutex_);
      } // HACK: direct access
      void signal() {
        p_cond.notify_one();
      }
  };

  int round(double x) {
    return int(std::floor(x + 0.5));
  }

  int div(int a, int b) {

    assert(b > 0);

    int div = a / b;
    if (a < 0 && a != b * div)
      div--; // fix buggy C semantics

    return div;
  }

  int sqrt(int n) {
    return int(std::sqrt(double(n)));
  }

  bool is_square(int n) {
    int i = sqrt(n);
    return i * i == n;
  }

  double rand_float() {
    return double(std::rand()) / (double(RAND_MAX) + 1.0);
  }

  int rand_int(int n) {
    assert(n > 0);
    return int(rand_float() * double(n));
  }

  int string_find(const std::string & s, std::string c) {
    return int(s.find(c));
  }

  bool string_case_equal(const std::string & s0, const std::string & s1) {

    if (s0.size() != s1.size())
      return false;

    for (int i = 0; i < int(s0.size()); i++) {
      if (std::tolower(s0[i]) != std::tolower(s1[i]))
        return false;
    }

    return true;
  }

  bool to_bool(const std::string & s) {
    if (string_case_equal(s, "true")) {
      return true;
    } else if (string_case_equal(s, "false")) {
      return false;
    } else {
      std::cerr << "not a boolean: \"" << s << "\"" << std::endl;
      std::exit(EXIT_FAILURE);
      return false;
    }
  }

  int64 to_int(const std::string & s) {
    std::stringstream ss(s);
    int64 n;
    ss >> n;
    return n;
  }

  std::string to_string(int n) {
    std::stringstream ss;
    ss << n;
    return ss.str();
  }

  std::string to_string(double x) {
    std::stringstream ss;
    ss << x;
    return ss.str();
  }

  void log(const std::string & s) {
    std::ofstream log_file("log.txt", std::ios_base::app);
    log_file << s << std::endl;
  }

  int lsb64(const uint64 u) {
    assert(u);
    return __builtin_ctzll(u);
  }
  int pop_cnt64(const uint64 u) {
    assert(u);
    return __builtin_popcountll(u);
  }
  //http://qiita.com/_meki/items/4328c98964ea33b0db0d
  template<typename List>
    void split(const std::string& s, const std::string& delim, List& result) {
      result.clear();
      std::string::size_type pos = 0;

      while (pos != std::string::npos) {
        std::string::size_type p = s.find(delim, pos);

        if (p == std::string::npos) {
          result.push_back(s.substr(pos));
          break;
        } else {
          result.push_back(s.substr(pos, p - pos));
        }

        pos = p + delim.size();
      }
    }
  std::string rtrim(const std::string s) {
    std::string str = s;
    const auto pos = str.rfind(' ');
    if (pos != std::string::npos) {
      str = str.substr(0, pos);
    }
    return str;
  }
  std::string timestamp() {
    std::time_t t = std::time(nullptr);
    char mbstr[256];
    if (std::strftime(mbstr, 256, "%Y%m%d-%H%M%S", std::localtime(&t))) {
      std::string str = mbstr;
      return str;
    }
    return "";
  }
  class TeeStream {
    public:
      TeeStream() {
        //ofs_.open("./log/" + timestamp() + ".log");
        ofs_.open("./log.log");
        ofs_ << "LOG_START " + timestamp() << std::endl;
      }
      template<typename T>
        TeeStream& operator <<(const T& t) {
          if (level_ == 1) {
            lock_.lock();

            std::cout << t;
            ofs_ << t;

            ofs_.flush();
            std::cout.flush();

            lock_.unlock();
          }
          return *this;
        }
      TeeStream& operator <<(std::ostream& (*f)(std::ostream&)) {
        std::cout << f;
        ofs_ << f;
        return *this;
      }

    private:
      std::ofstream ofs_;
      Lockable lock_;
      static constexpr int level_ = 1;
  };
  TeeStream Tee;

  // for non-AVX2 : software emulationによるpext実装(やや遅い。とりあえず動くというだけ。)
  inline uint64 my_pext(uint64 val, uint64 mask) {
    uint64 res = 0;
    for (uint64 bb = 1; mask; bb += bb) {
      if ((int64_t) val & (int64) mask & -(int64) mask) {
        res |= bb;
      }
      // マスクを1bitずつ剥がしていく実装なので処理時間がbit長に依存しない。
      // ゆえに、32bit用のpextを別途用意する必要がない。
      mask &= mask - 1;
    }
    return res;
  }
  inline uint32 PEXT32(uint32 a, uint32 b) {
    return (uint32) my_pext(a, b);
  }
  inline uint64 PEXT64(uint64 a, uint64 b) {
    return my_pext(a, b);
  }
  void init() {
    const auto seed = time(nullptr);
    util::Tee<<"seed:"<<seed<<std::endl;
    srand (seed);
  }
}
namespace input {

  class Input: public util::Waitable {

    bool volatile has_input_;
    bool eof_;
    std::string line_;

    public:

    Input() {
      has_input_ = false;
      eof_ = false;
    }

    bool has_input() const {
      return has_input_;
    }

    bool get_line(std::string & line) {

      lock();

      while (!has_input_) {
        wait();
      }

      bool line_ok = !eof_;
      if (line_ok)
        line = line_;

      has_input_ = false;
      signal();

      unlock();

      return line_ok;
    }

    void set_eof() {

      lock();

      while (has_input_) {
        wait();
      }

      eof_ = true;

      has_input_ = true;
      signal();

      unlock();
    }

    void set_line(std::string & line) {

      lock();

      while (has_input_) {
        wait();
      }

      line_ = line;

      has_input_ = true;
      signal();

      unlock();
    }

  };

  Input g_input;
  std::thread g_thread;

  void input_program(Input * input) {

    std::string line;

    while (std::getline(std::cin, line)) {
      input->set_line(line);
    }

    input->set_eof();
  }

  void init() {
    g_thread = std::thread(input_program, &g_input);
    g_thread.detach();
  }
}
namespace side {
  constexpr int SIZE = 2;
  enum {
    BLACK, WHITE
  };
  constexpr int opposit(int sd) {
    return sd ^ 1;
  }
  std::string to_string(const int sd) {
    return (sd) ? "WHITE" : "BLACK";
  }
}
namespace square {

  constexpr int FILE_SIZE = 9;
  constexpr int RANK_SIZE = 9;
  constexpr int SIZE = FILE_SIZE * RANK_SIZE;

  enum {
    FILE_1,
    FILE_2,
    FILE_3,
    FILE_4,
    FILE_5,
    FILE_6,
    FILE_7,
    FILE_8,
    FILE_9,
    FILE_LEFT = -1,
    FILE_RIGHT = 1,
  };

  enum {
    RANK_1,
    RANK_2,
    RANK_3,
    RANK_4,
    RANK_5,
    RANK_6,
    RANK_7,
    RANK_8,
    RANK_9,
    RANK_UP = -1,
    RANK_DOWN = 1,
  };

  enum {
    NONE = -1,
    SQ_11,
    SQ_12,
    SQ_13,
    SQ_14,
    SQ_15,
    SQ_16,
    SQ_17,
    SQ_18,
    SQ_19,
    SQ_21,
    SQ_22,
    SQ_23,
    SQ_24,
    SQ_25,
    SQ_26,
    SQ_27,
    SQ_28,
    SQ_29,
    SQ_31,
    SQ_32,
    SQ_33,
    SQ_34,
    SQ_35,
    SQ_36,
    SQ_37,
    SQ_38,
    SQ_39,
    SQ_41,
    SQ_42,
    SQ_43,
    SQ_44,
    SQ_45,
    SQ_46,
    SQ_47,
    SQ_48,
    SQ_49,
    SQ_51,
    SQ_52,
    SQ_53,
    SQ_54,
    SQ_55,
    SQ_56,
    SQ_57,
    SQ_58,
    SQ_59,
    SQ_61,
    SQ_62,
    SQ_63,
    SQ_64,
    SQ_65,
    SQ_66,
    SQ_67,
    SQ_68,
    SQ_69,
    SQ_71,
    SQ_72,
    SQ_73,
    SQ_74,
    SQ_75,
    SQ_76,
    SQ_77,
    SQ_78,
    SQ_79,
    SQ_81,
    SQ_82,
    SQ_83,
    SQ_84,
    SQ_85,
    SQ_86,
    SQ_87,
    SQ_88,
    SQ_89,
    SQ_91,
    SQ_92,
    SQ_93,
    SQ_94,
    SQ_95,
    SQ_96,
    SQ_97,
    SQ_98,
    SQ_99,
  };

  enum {
    INC_LEFT = -FILE_SIZE, INC_RIGHT = +FILE_SIZE, INC_UP = -1, INC_DOWN = +1
  };

  template<int sd = side::WHITE> int opposit_sq(int sq) {
    if (sd == side::BLACK) {
      return sq;
    } else {
      return (square::SIZE - sq - 1);
    }
  }
  int make(int fl, int rk) {
    assert(fl < FILE_SIZE);
    assert(rk < RANK_SIZE);
    return fl * FILE_SIZE + rk;
  }
  template<int sd> int make(int fl, int rk) {
    assert(fl < FILE_SIZE);
    assert(rk < RANK_SIZE);
    if (sd == side::BLACK) {
      return make(fl, rk);
    } else {
      return make(fl, std::abs(RANK_SIZE - rk - 1));
    }
  }
  int make(int fl, int rk, int sd) {
    if (sd == side::BLACK) {
      return make<side::BLACK>(fl, rk);
    } else {
      return make<side::WHITE>(fl, rk);
    }
  }
  int file(const int sq) {
    return sq / FILE_SIZE;
  }
  int rank(const int sq) {
    return sq % RANK_SIZE;
  }
  template<int sd> int rank(const int sq) {
    return rank(opposit_sq<sd>(sq));
  }
  template<int sd> int file(const int sq) {
    return file(opposit_sq<sd>(sq));
  }
  template<int sd> bool is_promotion(const int sq) {
    auto rk = rank(sq);
    if (sd == side::BLACK) {
      return rk <= RANK_3;
    } else {
      return rk >= RANK_7;
    }
  }
  bool is_promotion(const int sd, const int sq) {
    return (sd == side::BLACK) ?
      is_promotion<side::BLACK>(sq) : is_promotion<side::WHITE>(sq);
  }
  int from_sfen(int sq) {
    const auto f = file(sq);
    const auto r = RANK_SIZE - rank(sq) - 1;
    return make(r, f);
  }
  int from_string(const std::string& s) {
    assert(s.length() == 2);
    return make(s[0] - '1', s[1] - 'a');
  }
  std::string to_string(const int sq) {
    std::string s;
    s += '1' + file(sq);
    s += 'a' + rank(sq);
    return s;
  }
  std::string to_string2(const int sq) {
    std::string s;
    s += '1' + file(sq);
    s += '1' + rank(sq);
    return s;
  }
  bool is_valid_file(const int f) {
    return (f >= FILE_1 && f < FILE_SIZE);
  }
  bool is_valid_rank(const int r) {
    return (r >= RANK_1 && r < RANK_SIZE);
  }
  bool is_valid(const int f, const int r) {
    return (is_valid_rank(r)) && (is_valid_file(f));
  }
  bool is_valid_sq(const int sq) {
    return (sq >= 0) && (sq < square::SIZE);
  }
  template<int sd> int side_rank(const int rank) {
    return (sd == side::BLACK) ? rank : (square::RANK_SIZE - rank - 1);
  }
}
namespace piece {

  constexpr int PAWN_VALUE    = 100;
  constexpr int LANCE_VALUE   = 300;
  constexpr int KNIGHT_VALUE  = 300;
  constexpr int SILVER_VALUE  = 400;
  constexpr int GOLD_VALUE    = 500;
  constexpr int BISHOP_VALUE  = 600;
  constexpr int ROOK_VALUE    = 700;
  constexpr int PPAWN_VALUE   = 600;
  constexpr int PLANCE_VALUE  = 500;
  constexpr int PKNIGHT_VALUE = 500;
  constexpr int PSILVER_VALUE = 500;
  constexpr int PBISHOP_VALUE = 800;
  constexpr int PROOK_VALUE   = 1000;
  constexpr int KING_VALUE    = 10000;// for SEE

  static constexpr int g_value[] = {
    0,
    PAWN_VALUE,
    LANCE_VALUE,
    KNIGHT_VALUE,
    SILVER_VALUE,
    BISHOP_VALUE,
    ROOK_VALUE,
    GOLD_VALUE,
    KING_VALUE,
    PPAWN_VALUE,
    PLANCE_VALUE,
    PKNIGHT_VALUE,
    PSILVER_VALUE,
    PBISHOP_VALUE,
    PROOK_VALUE,
  };

  static constexpr int g_prom_value[] = {
    0,
    PPAWN_VALUE - PAWN_VALUE,
    PLANCE_VALUE - LANCE_VALUE,
    PKNIGHT_VALUE - KNIGHT_VALUE,
    PSILVER_VALUE - SILVER_VALUE,
    PBISHOP_VALUE - BISHOP_VALUE,
    PROOK_VALUE - ROOK_VALUE,
    GOLD_VALUE - GOLD_VALUE,
    KING_VALUE - KING_VALUE,
    PPAWN_VALUE - PAWN_VALUE,
    PLANCE_VALUE - LANCE_VALUE,
    PKNIGHT_VALUE - KNIGHT_VALUE,
    PSILVER_VALUE - SILVER_VALUE,
    PBISHOP_VALUE - BISHOP_VALUE,
    PROOK_VALUE - ROOK_VALUE,
  };
  static constexpr int g_ex_value[] = {
    0,
    PAWN_VALUE + PAWN_VALUE,
    LANCE_VALUE + LANCE_VALUE,
    KNIGHT_VALUE + KNIGHT_VALUE,
    SILVER_VALUE + SILVER_VALUE,
    BISHOP_VALUE + BISHOP_VALUE,
    ROOK_VALUE + ROOK_VALUE,
    GOLD_VALUE + GOLD_VALUE,
    KING_VALUE,
    PPAWN_VALUE + PAWN_VALUE,
    PLANCE_VALUE + LANCE_VALUE,
    PKNIGHT_VALUE + KNIGHT_VALUE,
    PSILVER_VALUE + SILVER_VALUE,
    PBISHOP_VALUE + BISHOP_VALUE,
    PROOK_VALUE + ROOK_VALUE,
  };


  constexpr int SIZE = 16;
  constexpr int SIDE_SIZE = 32;
  constexpr int PROM_FLAG = 8;
  const std::string Char = ". P L N S B R G K +P+L+N+S+B+R+G+";
  const std::string Sfen_Char =
    ". P L N S B R G K +P+L+N+S+B+R+G+.p l n s b r g k +p+l+n+s+b+r+g+k";
  enum Piece {
    NONE,
    PAWN,
    LANCE,
    KNIGHT,
    SILVER,
    BISHOP,
    ROOK,
    GOLD,
    KING,
    PPAWN,
    PLANCE,
    PKNIGHT,
    PSILVER,
    PBISHOP,
    PROOK,
    GOLDS,
  };
  enum SidePiece {
    BLACK_NONE,
    BLACK_PAWN,
    BLACK_LANCE,
    BLACK_KNIGHT,
    BLACK_SILVER,
    BLACK_BISHOP,
    BLACK_ROOK,
    BLACK_GOLD,
    BLACK_KING,
    BLACK_PPAWN,
    BLACK_PLANCE,
    BLACK_PKNIGHT,
    BLACK_PSILVER,
    BLACK_PBISHOP,
    BLACK_PROOK,
    BLACK_GOLDS,
    WHITE_NONE,
    WHITE_PAWN,
    WHITE_LANCE,
    WHITE_KNIGHT,
    WHITE_SILVER,
    WHITE_BISHOP,
    WHITE_ROOK,
    WHITE_GOLD,
    WHITE_KING,
    WHITE_PPAWN,
    WHITE_PLANCE,
    WHITE_PKNIGHT,
    WHITE_PSILVER,
    WHITE_PBISHOP,
    WHITE_PROOK,
    WHITE_GOLDS,
  };
  constexpr bool is_slider(const int pc) {
    return (pc == ROOK || pc == BISHOP || pc == LANCE || pc == PBISHOP
        || pc == PROOK);
  }
  int score(const int pc) { //for MVV/LVA
    static constexpr int value[SIZE] = { -99, 1, 2, 3, 4, 6, 7, 5, 100, 5, 5, 5,
      5, 8, 9, 10, };
    assert(pc < SIZE);
    return value[pc];
  }
  constexpr int value(const int pc) {
    return g_value[pc];
  }
  constexpr int ex_value(const int pc) {
    return g_ex_value[pc];
  }
  constexpr int prom_value(const int pc) {
    return g_prom_value[pc];
  }
  int make(const int pc, const int sd) {
    assert(pc < SIZE);
    assert(pc != NONE);
    assert(sd < side::SIZE);
    return (sd << 4) | pc;
  }
  int piece(const int p32) {
    assert(p32 < SIDE_SIZE);
    return p32 & BLACK_GOLDS;
  }
  int side(const int p32) {
    assert(p32 < SIDE_SIZE);
    return (p32 >> 4) & 1;
  }
  int from_string(const std::string str) {
    return util::string_find(Char, str) / 2;
  }
  std::string to_char(const int pc) {
    return Char.substr(pc * 2, 2);
  }
  int from_sfen(const std::string str) {
    return util::string_find(Sfen_Char, str) / 2;
  }
  std::string to_sfen(const int p32) {
    assert(p32 < SIDE_SIZE);
    return Sfen_Char.substr(p32 * 2, 2);
  }
  std::string to_str(const int p32) {
    static std::string str[] = { " ・", " 歩", " 香", " 桂", " 銀", " 角", " 飛", " 金",
      " 玉", " と", " 杏", " 圭", " 全", " 馬", " 龍", " 　", " ・", "v歩", "v香",
      "v桂", "v銀", "v角", "v飛", "v金", "v玉", "vと", "v杏", "v圭", "v全", "v馬",
      "v龍" };
    return str[p32];
  }
  bool is_valid(const int pc) {
    return pc >= piece::NONE && pc < piece::GOLDS;
  }
  bool is_valid_piece(const int pc) {
    return pc > piece::NONE && pc < piece::GOLDS;
  }
  int prom(const int pc) {
    return pc | PROM_FLAG;
  }
  int unprom(const int pc) {
    return pc & (PROM_FLAG - 1);
  }
  constexpr bool is_gold(const int pc) {
    return (pc == piece::GOLD || pc == piece::PPAWN || pc == piece::PLANCE
        || pc == piece::PKNIGHT || pc == piece::PSILVER);
  }
  constexpr bool can_prom(const int pc) {
    return (pc == PAWN || pc == LANCE || pc == KNIGHT || pc == SILVER
        || pc == BISHOP || pc == ROOK);
  }

}

namespace move {

  // move structure
  //xxxxxxxx xxxxxxxx xxxxxxxx x1111111  destination
  //xxxxxxxx xxxxxxxx xx111111 1xxxxxxx  starting square(drop move = square::SIZE)
  //xxxxxxxx xxxxxxxx x1xxxxxx xxxxxxxx  flag for promotion
  //xxxxxxxx xxxxx111 1xxxxxxx xxxxxxxx  piece to move
  //xxxxxxxx x1111xxx xxxxxxxx xxxxxxxx  captured piece


  // move16 structure
  //xxxxxxxx xxxxxxxx xxxxxxxx x1111111  destination
  //xxxxxxxx xxxxxxxx xx111111 1xxxxxxx  starting square(drop move = square::SIZE + piece)
  //xxxxxxxx xxxxxxxx x1xxxxxx xxxxxxxx  flag for promotion

  constexpr int TO_SHIFT = 0;
  constexpr int FROM_SHIFT = 7;
  constexpr int PROM_SHIFT = 14;
  constexpr int PIECE_SHIFT = 15;
  constexpr int CAP_SHIFT = 19;

  constexpr int BITS = 24;
  constexpr int SIZE = 1 << BITS;
  constexpr int MASK = SIZE - 1;

  constexpr int SCORE_BITS = 30;//64 - BITS;
  constexpr int SCORE_SIZE = 1 << SCORE_BITS;
  constexpr uint32 SCORE_MASK = SCORE_SIZE - 1;

  constexpr uint32 MOVE16_MASK = 0x7FFF;

  enum Move {
    NONE = 0, NULL_ = 1
  };
  //move
  int make(const int f, const int t, const int pc, const int cp, const bool pp =
      false) {
    //assert(square::is_valid_sq(f));
    //assert(square::is_valid_sq(t));
    //assert(piece::is_valid_piece(pc));
    //assert(piece::is_valid(cp));

    return (f << FROM_SHIFT) | (t << TO_SHIFT) | (pc << PIECE_SHIFT)
      | (cp << CAP_SHIFT) | (static_cast<int>(pp) << PROM_SHIFT);

  }
  //drop
  int make(const int t, const int pc) {
    //assert(square::is_valid_sq(t));
    //assert(piece::is_valid_piece(pc));

    return (square::SIZE << FROM_SHIFT) | (t << TO_SHIFT) | (pc << PIECE_SHIFT);

  }
  int from(const int mv) {
    return (mv >> FROM_SHIFT) & 0x7f;
  }
  int to(const int mv) {
    return (mv >> TO_SHIFT) & 0x7f;
  }
  int piece(const int mv) {
    return (mv >> PIECE_SHIFT) & 0xf;
  }
  int cap(const int mv) {
    return (mv >> CAP_SHIFT) & 0xf;
  }
  bool prom(const int mv) {
    return ((mv & (1 << PROM_SHIFT)) != 0);
  }
  bool is_drop(const int mv) {
    return from(mv) >= square::SIZE;
  }
  std::string to_can(const int mv) {
    if (mv == NONE || mv == NULL_) {
      return "0000";
    }
    std::string s;
    if (is_drop(mv)) {
      s += util::rtrim(piece::to_char(piece(mv)));
      s += "*";
      s += square::to_string(to(mv));
    } else {
      s += square::to_string(from(mv));
      s += square::to_string(to(mv));
      if (move::prom(mv)) {
        s += "+";
      }
    }
    return s;
  }
  std::string to_can2(const int mv) {
    if (mv == NONE || mv == NULL_) {
      return "0000";
    }
    std::string s;
    if (is_drop(mv)) {
      s += util::rtrim(piece::to_char(piece(mv)));
      s += "*";
      s += square::to_string2(to(mv));
    } else {
      s += square::to_string2(from(mv));
      s += square::to_string2(to(mv));
      if (move::prom(mv)) {
        s += "+";
      }
    }
    return s;
  }

}
namespace hand {
  //handの構成
  //     rook      bishop      gold       silver     knight      lance      pawn
  //1bit 2bit 1bit 2bit 1bit   3bit 1bit  3bit 1bit  3bit  1bit  3bit 1bit  5bit
  //g  p  l  n   s   b   r
  constexpr uint32 hand_shift[] = { 0, 6, 10, 14, 22, 25, 18 };

  constexpr uint32 hand_mask[] = { 0x1fu << hand_shift[0], 0x7u << hand_shift[1],
    0x7u << hand_shift[2], 0x7u << hand_shift[3], 0x3u << hand_shift[4],
    0x3u << hand_shift[5], 0x7u << hand_shift[6] };

  constexpr uint32 hand_inc[] = { 1u << hand_shift[0], 1u << hand_shift[1], 1u
    << hand_shift[2], 1u << hand_shift[3], 1u << hand_shift[4], 1u
      << hand_shift[5], 1u << hand_shift[6] };

  inline int to_hp(const int pc) {
    assert(piece::is_valid_piece(pc));
    return pc - 1;
  }
  inline int num(const uint32 hand, const int pc) {
    const int hp = to_hp(pc);
    return (hand & hand_mask[hp]) >> hand_shift[hp];
  }
  inline bool has(const uint32 hand, const int pc) {
    const int hp = to_hp(pc);
    return (hand & hand_mask[hp]) != 0;
  }

  template<bool inc> int change(const uint32 hand, const int pc) {
    const int flag = (inc) ? 1 : -1;
    return hand + flag * hand_inc[to_hp(pc)];
  }
  inline bool empty(const uint32 hand) {
    return hand == 0;
  }
  std::string to_string(const uint32 hand) {
    std::string s = "";
    for (auto pc = int(piece::PAWN); pc <= int(piece::GOLD); pc++) {
      s += util::rtrim(piece::to_char(pc)) + "/"
        + util::to_string(num(hand, pc)) + " ";
    }
    return s;
  }
  bool is_superior(const uint32 hand1, const uint32 hand2) {
    static constexpr uint32
      hand_overflow_mask	= hand_mask[0] | hand_mask[1] | hand_mask[2]
      | hand_mask[3] | hand_mask[4] | hand_mask[5] | hand_mask[6];
    return ((hand1 - hand2) & hand_overflow_mask) == 0;
  }

}
namespace bit {
  class Bitboard {
    private:
      std::array<uint64, 2> p_;
    public:
      Bitboard() {
      }
      Bitboard operator ~() const {
        Bitboard tmp;
        tmp.p_[0] = ~p(0);
        tmp.p_[1] = ~p(1);
        return tmp;
      }
      Bitboard operator &=(const Bitboard& rhs) {
        p_[0] &= rhs.p(0);
        p_[1] &= rhs.p(1);
        return *this;
      }
      Bitboard operator |=(const Bitboard& rhs) {
        p_[0] |= rhs.p(0);
        p_[1] |= rhs.p(1);
        return *this;
      }
      Bitboard operator ^=(const Bitboard& rhs) {
        p_[0] ^= rhs.p(0);
        p_[1] ^= rhs.p(1);
        return *this;
      }
      Bitboard operator +=(const Bitboard& rhs) {
        p_[0] += rhs.p(0);
        p_[1] += rhs.p(1);
        return *this;
      }
      Bitboard operator <<=(const int i) {
        p_[0] <<= i;
        p_[1] <<= i;
        return *this;
      }
      Bitboard operator >>=(const int i) {
        p_[0] >>= i;
        p_[1] >>= i;
        return *this;
      }
      Bitboard operator &(const Bitboard& rhs) const {
        return Bitboard(*this) &= rhs;
      }
      Bitboard operator |(const Bitboard& rhs) const {
        return Bitboard(*this) |= rhs;
      }
      Bitboard operator ^(const Bitboard& rhs) const {
        return Bitboard(*this) ^= rhs;
      }
      Bitboard operator +(const Bitboard& rhs) const {
        return Bitboard(*this) += rhs;
      }
      Bitboard operator <<(const int i) const {
        return Bitboard(*this) <<= i;
      }
      Bitboard operator >>(const int i) const {
        return Bitboard(*this) >>= i;
      }
      bool operator ==(const Bitboard& rhs) const {
        return ((p(0) == rhs.p(0)) && (p(1) == rhs.p(1)));
      }
      bool operator !=(const Bitboard& rhs) const {
        return !(*this == rhs);
      }
      uint64 p(const int index) const {
        return p_[index];
      }
      template<int index> void set(const uint64 u) {
        p_[index] = u;
      }
      void set(const int index, const uint64 u) {
        (!index) ? set<0>(u) : set<1>(u);
      }
      void init() {
        p_[0] = p_[1] = 0ull;
      }
      uint64 merge() const {
        return (p(0) | p(1));
      }
      bool is_empty() const {
        return merge() == 0ull;
      }
      int pop_cnt() const {
        auto num = 0;
        if (p(0)) {
          num += util::pop_cnt64(p(0));
        }
        if (p(1)) {
          num += util::pop_cnt64(p(1));
        }
        return num;
      }
      template<bool is_del = true> int lsb_right() {
        assert(!is_empty());
        const auto ret = (util::lsb64(this->p(0)));
        if (is_del) {
          p_[0] &= p_[0] - 1;
        }
        assert(ret >= 0 && ret <= 63);
        return ret;
      }
      template<bool is_del = true> int lsb_left() {
        assert(!is_empty());
        const auto ret = (util::lsb64(this->p(1)));
        if (is_del) {
          p_[1] &= p_[1] - 1;
        }
        assert(ret >= 0 && ret <= 17);
        return (ret + 63);
      }
      template<bool is_del = true> int lsb() {
        assert(!is_empty());
        if (this->p(0)) {
          return lsb_right<is_del>();
        }
        return lsb_left<is_del>();
      }
      friend std::ostream& operator<<(std::ostream& os, const Bitboard& b) {
        os << b.p(0) << std::endl;
        os << b.p(1) << std::endl;
        os << "-----------------" << std::endl;
        for (int rank = 0; rank < 9; rank++) {
          for (int file = 8; file >= 0; file--) {
            int xy = rank + file * 9;
            (b.is_set(xy)) ? os << "1," : os << "0,";
          }
          os << std::endl;
        }
        return os;
      }
      bool is_set(const int) const;
      Bitboard operator &=(const int);
      Bitboard operator |=(const int);
      Bitboard operator ^=(const int);
      Bitboard operator &(const int xy) const {
        return Bitboard(*this) &= xy;
      }
      Bitboard operator |(const int xy) const {
        return Bitboard(*this) |= xy;
      }
      Bitboard operator ^(const int xy) const {
        return Bitboard(*this) ^= xy;
      }
      void clear(const int sq) {
        assert(is_set(sq));
        (*this) ^= sq;
      }
      void set(const int sq) {
        (*this) |= sq;
      }
      static int select(const int xy) {
        return static_cast<int>(xy >= 63);
      }
  };

  std::array<Bitboard, square::SIZE> g_mask;
  std::array<Bitboard, square::SIZE> g_rook_mask;
  std::array<Bitboard, square::SIZE> g_bishop_mask;
  std::array<Bitboard, square::FILE_SIZE> g_file_mask;
  std::array<Bitboard, square::RANK_SIZE> g_rank_mask;

  std::array<Bitboard, side::SIZE> g_prom; //1~3
  std::array<Bitboard, side::SIZE> g_middle; //4~9
  bit::Bitboard g_all_one;
  std::array<std::array<Bitboard, 1 << 9>, side::SIZE> g_double_pawn_mask;

  bool Bitboard::is_set(const int sq) const {
    return !(((*this) & g_mask[sq]).is_empty());
  }

  Bitboard Bitboard::operator &=(const int xy) {
    p_[0] &= g_mask[xy].p(0);
    p_[1] &= g_mask[xy].p(1);
    return *this;
  }

  Bitboard Bitboard::operator |=(const int xy) {
    p_[0] |= g_mask[xy].p(0);
    p_[1] |= g_mask[xy].p(1);
    return *this;
  }

  Bitboard Bitboard::operator ^=(const int xy) {
    p_[0] ^= g_mask[xy].p(0);
    p_[1] ^= g_mask[xy].p(1);
    return *this;
  }

  void init() {
    //mask init
    for (auto i = 0u; i < g_mask.size(); i++) {
      g_mask[i].init();
      (!bit::Bitboard::select(i)) ?
        g_mask[i].set(0, (U64(1) << i)) :
        g_mask[i].set(1, (U64(1) << (i - 63)));
    }
    //rank mask init
    //file_mask init
    for (auto &bb : g_file_mask) {
      bb.init();
    }
    for (auto &bb : g_rank_mask) {
      bb.init();
    }
    for (auto i = 0u; i < square::SIZE; i++) {
      const auto file = square::file(i);
      const auto rank = square::rank(i);
      g_file_mask[file] |= i;
      g_rank_mask[rank] |= i;
    }
    //rook mask bishop mask int
    auto init_func = [](std::array<bit::Bitboard,square::SIZE> &bb_list,
        std::array<int,2>file_inc,
        std::array<int,2>rank_inc,
        bool is_bishop) {
      for(auto sq = 0u; sq < bb_list.size(); sq++) {
        auto file = square::file(sq);
        auto rank = square::rank(sq);
        for(auto f_inc : file_inc) {
          for(auto r_inc : rank_inc) {
            auto f = file;
            auto r = rank;
            while(true) {
              f += f_inc;
              r += r_inc;
              if(square::is_valid(f,r)) {
                bb_list[sq] |= square::make(f,r);
              } else {
                break;
              }
            }
          }
        }
        if(is_bishop) {
          bb_list[sq] &= ~g_file_mask[square::FILE_1];
          bb_list[sq] &= ~g_file_mask[square::FILE_9];
          bb_list[sq] &= ~g_rank_mask[square::RANK_1];
          bb_list[sq] &= ~g_rank_mask[square::RANK_9];
        } else {
          if(square::file(sq) != square::FILE_1) {
            bb_list[sq] &= ~g_file_mask[square::FILE_1];
          }
          if(square::file(sq) != square::FILE_9) {
            bb_list[sq] &= ~g_file_mask[square::FILE_9];
          }
          if(square::rank(sq) != square::RANK_1) {
            bb_list[sq] &= ~g_rank_mask[square::RANK_1];
          }
          if(square::rank(sq) != square::RANK_9) {
            bb_list[sq] &= ~g_rank_mask[square::RANK_9];
          }
        }
      }
    };
    for (auto &bb : g_rook_mask) {
      bb.init();
    }
    for (auto &bb : g_bishop_mask) {
      bb.init();
    }
    init_func(g_rook_mask, { 0, 0 }, { -1, 1 }, false);
    init_func(g_rook_mask, { -1, 1 }, { 0, 0 }, false);
    init_func(g_bishop_mask, { 1, 1 }, { -1, 1 }, true);
    init_func(g_bishop_mask, { -1, -1 }, { -1, 1 }, true);

    //prom middle
    g_prom[side::BLACK].init();
    g_prom[side::WHITE].init();
    g_middle[side::BLACK].init();
    g_middle[side::WHITE].init();
    for (auto sq = 0; sq < square::SIZE; sq++) {
      const auto rank = square::rank(sq);
      if (rank == square::RANK_1 || rank == square::RANK_2
          || rank == square::RANK_3) {
        g_prom[side::BLACK].set(sq);
      }
      if (rank == square::RANK_7 || rank == square::RANK_8
          || rank == square::RANK_9) {
        g_prom[side::WHITE].set(sq);
      }
      if ( rank == square::RANK_4 || rank == square::RANK_5
          || rank == square::RANK_6 || rank == square::RANK_7
          || rank == square::RANK_8 || rank == square::RANK_9) {
        g_middle[side::BLACK].set(sq);
      }
      if (rank == square::RANK_1 || rank == square::RANK_2
          || rank == square::RANK_3 || rank == square::RANK_4
          || rank == square::RANK_5 || rank == square::RANK_6) {
        g_middle[side::WHITE].set(sq);
      }
    }
    //double pawn mask
    for (auto index = 0; index < (1 << square::FILE_SIZE); index++) {
      g_double_pawn_mask[side::BLACK][index].init();
      g_double_pawn_mask[side::WHITE][index].init();
      for (auto shift = 0; shift < square::FILE_SIZE; shift++) {
        if (!(index & (1 << shift))) {
          g_double_pawn_mask[side::BLACK][index] |= g_file_mask[shift];
          g_double_pawn_mask[side::WHITE][index] |= g_file_mask[shift];
        }
      }
      g_double_pawn_mask[side::BLACK][index] &= ~g_rank_mask[square::RANK_1];
      g_double_pawn_mask[side::WHITE][index] &= ~g_rank_mask[square::RANK_9];
    }
    //all_one
    g_all_one.init();
    for (auto sq = 0; sq < square::SIZE; sq++) {
      g_all_one.set(sq);
    }
  }
}
namespace hash {
  constexpr int TURN = piece::SIDE_SIZE * square::SIZE;
  constexpr int SIZE = 1 << 12;
  static_assert(SIZE > TURN,"hash size error");
  hash_t g_rand[SIZE];
  hash_t rand_64() {
    hash_t rand = 0;
    for (auto i = 0; i < 4; i++) {
      rand = (rand << 16) | util::rand_int(1 << 16);
    }
    return rand;
  }
  hash_t rand_key(int index) {
    return g_rand[index];
  }
  hash_t piece_key(int p32, int sq) {
    return rand_key(p32 * square::SIZE + sq);
  }
  hash_t turn_key(int turn) {
    return (turn == side::WHITE) ? rand_key(TURN) : 0;
  }
  hash_t turn_flip() {
    return rand_key(TURN);
  }
  int64 index(hash_t key) {
    return int64(key);
  }
  uint32 lock(hash_t key) {
    return uint32( (hash_t)key >> 32);
  }
  void init() {
    for (auto &rd : g_rand) {
      rd = rand_64();
    }
  }
}
namespace castling {
  void init() {
  }
}
namespace attack {

  struct Attacks {
    int size;
    std::array<int, 2> square;
    bit::Bitboard avoid;
    bit::Bitboard pinned;
  };

  constexpr int BISHOP_ATTACK_SIZE = 20224 + 1;
  constexpr int ROOK_ATTACK_SIZE = 495616 + 1;

  constexpr std::array<int, square::SIZE> g_lance_shift = { 1, 1, 1, 1, 1, 1, 1,
    1, 1, 10, 10, 10, 10, 10, 10, 10, 10, 10, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 28, 28, 28, 28, 28, 28, 28, 28, 28, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 46, 46, 46, 46, 46, 46, 46, 46, 46, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 10, 10, 10, 10, };
  std::array<bit::Bitboard, BISHOP_ATTACK_SIZE> g_bishop_attacks;
  std::array<bit::Bitboard, ROOK_ATTACK_SIZE> g_rook_attacks;
  std::array<int, square::SIZE> g_bishop_offsets;
  std::array<int, square::SIZE> g_rook_offsets;

  typedef std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> bw_square_t;
  bw_square_t g_pawn_attacks;
  bw_square_t g_knight_attacks;
  bw_square_t g_silver_attacks;
  bw_square_t g_gold_attacks;
  std::array<bit::Bitboard, square::SIZE> g_king_attacks;
  std::array<std::array<std::array<bit::Bitboard, 128>, square::SIZE>, side::SIZE> g_lance_attacks;
  std::array<std::array<bit::Bitboard, square::SIZE>, square::SIZE> g_between;
  std::array<std::array<bit::Bitboard, square::SIZE>, square::SIZE> g_behind;

  std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> g_gold_check_table;
  std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> g_silver_check_table;
  std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> g_knight_check_table;
  std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> g_lance_check_table;
  std::array<std::array<bit::Bitboard, square::SIZE>, side::SIZE> g_pawn_check_table;

  void valid_set(bit::Bitboard &bb, int f, int r) {

    if (square::is_valid(f, r)) {
      auto sq = square::make(f, r);
      bb |= sq;
    }
  }

  bit::Bitboard init_pawn_attacks(int sd, int f, int r) {

    bit::Bitboard bb;
    bb.init();

    auto opposit = (sd == side::BLACK) ? 1 : -1;
    auto file = f;
    auto rank = r + square::INC_UP * opposit;
    valid_set(bb, file, rank);

    return bb;
  }
  bit::Bitboard init_knight_attacks(int sd, int f, int r) {

    bit::Bitboard bb;
    bb.init();

    auto opposit = (sd == side::BLACK) ? 1 : -1;
    auto file = f + square::FILE_LEFT;
    auto rank = r + square::RANK_UP * 2 * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_RIGHT;
    rank = r + square::RANK_UP * 2 * opposit;
    valid_set(bb, file, rank);

    return bb;
  }
  bit::Bitboard init_silver_attacks(int sd, int f, int r) {

    bit::Bitboard bb;
    bb.init();

    auto opposit = (sd == side::BLACK) ? 1 : -1;
    auto file = f + square::FILE_LEFT;
    auto rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f;
    rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_RIGHT;
    rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_LEFT;
    rank = r + square::RANK_DOWN * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_RIGHT;
    rank = r + square::RANK_DOWN * opposit;
    valid_set(bb, file, rank);

    return bb;
  }
  bit::Bitboard init_gold_attacks(int sd, int f, int r) {

    bit::Bitboard bb;
    bb.init();

    auto opposit = (sd == side::BLACK) ? 1 : -1;
    auto file = f + square::FILE_LEFT;
    auto rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f;
    rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_RIGHT;
    rank = r + square::RANK_UP * opposit;
    valid_set(bb, file, rank);

    file = f + square::FILE_LEFT;
    rank = r;
    valid_set(bb, file, rank);

    file = f + square::FILE_RIGHT;
    rank = r;
    valid_set(bb, file, rank);

    file = f;
    rank = r + square::INC_DOWN * opposit;
    valid_set(bb, file, rank);

    return bb;
  }
  inline bit::Bitboard index_to_occ(const int index, const int bits,
      const bit::Bitboard &bb) {
    bit::Bitboard ret_bb;
    bit::Bitboard mask = bb;
    ret_bb.init();

    assert(bits == mask.pop_cnt());
    for (auto i = 0; i < bits; i++) {
      const auto sq = mask.lsb();
      if (index & (1 << i)) {
        ret_bb |= sq;
      }
    }
    return ret_bb;
  }
  inline uint64 occ_to_index(const bit::Bitboard &bb, const bit::Bitboard &mask) {
    return util::my_pext(bb.merge(), mask.merge());
  }
  inline bit::Bitboard calc_slider_att(const int sq, const bit::Bitboard &occ,
      const bool is_bishop) {
    bit::Bitboard ret_bb;
    ret_bb.init();
    std::array<std::array<int, 4>, 2> bishop_dir_list = { { { 1, -1, 1, -1 }, {
      1, 1, -1, -1 } } };
    std::array<std::array<int, 4>, 2> rook_dir_list = { { { 1, -1, 0, 0 }, { 0,
      0, 1, -1 } } };
    for (auto i = 0; i < 4; i++) {
      const auto dir_file =
        (is_bishop) ? bishop_dir_list[0][i] : rook_dir_list[0][i];
      const auto dir_rank =
        (is_bishop) ? bishop_dir_list[1][i] : rook_dir_list[1][i];
      const auto file = square::file(sq);
      const auto rank = square::rank(sq);
      for (auto f = file + dir_file, r = rank + dir_rank;
          square::is_valid(f, r); f += dir_file, r += dir_rank) {
        auto to_sq = square::make(f, r);
        ret_bb |= to_sq;
        if (occ.is_set(to_sq)) {
          break;
        }
      }
    }
    return ret_bb;
  }

  bit::Bitboard between_debug(const int from, const int to) {

    bit::Bitboard b;
    b.init();
    const auto is_bishop = g_bishop_attacks[g_bishop_offsets[from]].is_set(to);
    const auto is_rook = g_rook_attacks[g_rook_offsets[from]].is_set(to);
    if (is_bishop || is_rook) {
      const auto f_file = square::file(from);
      const auto f_rank = square::rank(from);
      const auto t_file = square::file(to);
      const auto t_rank = square::rank(to);
      auto dir_file = 0;
      auto dir_rank = 0;
      if (is_bishop) {
        dir_file = (f_file < t_file) ? +1 : -1;
        dir_rank = (f_rank < t_rank) ? +1 : -1;
      } else {
        if (f_file == t_file) {
          dir_file = 0;
          dir_rank = (f_rank < t_rank) ? +1 : -1;
        } else {
          dir_file = (f_file < t_file) ? +1 : -1;
          dir_rank = 0;
        }
      }
      for (auto f = f_file + dir_file, r = f_rank + dir_rank;
          square::is_valid(f, r); f += dir_file, r += dir_rank) {
        auto sq = square::make(f, r);
        if (sq == to) {
          break;
        }
        b |= sq;
      }
    }
    return b;
  }
  bit::Bitboard behind_debug(const int from, const int to) {

    bit::Bitboard b;
    b.init();
    const auto is_bishop = g_bishop_attacks[g_bishop_offsets[from]].is_set(to);
    const auto is_rook = g_rook_attacks[g_rook_offsets[from]].is_set(to);
    if (is_bishop || is_rook) {
      const auto f_file = square::file(from);
      const auto f_rank = square::rank(from);
      const auto t_file = square::file(to);
      const auto t_rank = square::rank(to);
      auto dir_file = 0;
      auto dir_rank = 0;
      if (is_bishop) {
        dir_file = (f_file < t_file) ? +1 : -1;
        dir_rank = (f_rank < t_rank) ? +1 : -1;
      } else {
        if (f_file == t_file) {
          dir_file = 0;
          dir_rank = (f_rank < t_rank) ? +1 : -1;
        } else {
          dir_file = (f_file < t_file) ? +1 : -1;
          dir_rank = 0;
        }
      }
      auto found = false;
      for (auto f = f_file + dir_file, r = f_rank + dir_rank;
          square::is_valid(f, r); f += dir_file, r += dir_rank) {
        auto sq = square::make(f, r);
        if (found) {
          b |= sq;
        }
        if (sq == to) {
          found = true;
        }
      }
      //		util::Tee<<"from:"<<from<<" to:"<<to<<std::endl;
      //		util::Tee<<b<<std::endl;
      //		util::Tee<<g_bishop_attacks[g_bishop_offsets[from]]<<std::endl;
    }
    return b;
  }
  bit::Bitboard gold_check_debug(const int sd, const int sq) {
    bit::Bitboard ret;
    ret.init();
    const auto xd = side::opposit(sd);

    bit::Bitboard bb = g_gold_attacks[xd][sq];
    while(!bb.is_empty()){
      const auto to = bb.lsb();
      ret |= g_gold_attacks[xd][to];
    }
    return ret;
  }
  bit::Bitboard silver_check_debug(const int sd, const int sq) {
    bit::Bitboard ret;
    ret.init();
    const auto xd = side::opposit(sd);

    //noprom→noprom
    {
      bit::Bitboard bb = g_silver_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        ret |= g_silver_attacks[xd][to];
      }
    }
    //noprom→prom
    {
      bit::Bitboard bb = g_gold_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        if(square::is_promotion(sd,to)) {
          ret |= g_silver_attacks[xd][to];
        }
      }
    }
    //prom→prom
    //prom→noprom
    {
      bit::Bitboard bb = g_gold_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        ret |= (g_silver_attacks[xd][to] & bit::g_prom[sd]);
      }
    }

    return ret;
  }
  bit::Bitboard knight_check_debug(const int sd, const int sq) {
    bit::Bitboard ret;
    ret.init();
    const auto xd = side::opposit(sd);

    //noprom→noprom
    {
      bit::Bitboard bb = g_knight_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        ret |= g_knight_attacks[xd][to];
      }
    }
    //noprom→prom
    {
      bit::Bitboard bb = g_gold_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        if(square::is_promotion(sd,to)) {
          ret |= g_knight_attacks[xd][to];
        }
      }
    }

    return ret;
  }
  bit::Bitboard lance_check_debug(const int sd, const int sq) {
    bit::Bitboard ret;
    ret.init();
    const auto xd = side::opposit(sd);

    //noprom→noprom
    {
      bit::Bitboard bb = g_lance_attacks[xd][sq][0];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        ret |= g_lance_attacks[xd][to][0];
      }
    }
    //noprom→prom
    {
      bit::Bitboard bb = g_gold_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        if(square::is_promotion(sd,to)) {
          ret |= g_lance_attacks[xd][to][0];
        }
      }
    }

    return ret;
  }

  bit::Bitboard pawn_check_debug(const int sd, const int sq) {
    bit::Bitboard ret;
    ret.init();
    const auto xd = side::opposit(sd);

    //noprom→noprom
    {
      bit::Bitboard bb = g_pawn_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        ret |= g_pawn_attacks[xd][to];
      }
    }
    //noprom→prom
    {
      bit::Bitboard bb = g_gold_attacks[xd][sq];
      while(!bb.is_empty()){
        const auto to = bb.lsb();
        if(square::is_promotion(sd,to)) {
          ret |= g_pawn_attacks[xd][to];
        }
      }
    }

    return ret;
  }

  void init() {

    //attack table
    for (auto sq = 0; sq < square::SIZE; sq++) {
      for (auto sd = 0; sd < side::SIZE; sd++) {
        auto file = square::file(sq);
        auto rank = square::rank(sq);
        g_pawn_attacks[sd][sq] = init_pawn_attacks(sd, file, rank);
        g_knight_attacks[sd][sq] = init_knight_attacks(sd, file, rank);
        g_silver_attacks[sd][sq] = init_silver_attacks(sd, file, rank);
        g_gold_attacks[sd][sq] = init_gold_attacks(sd, file, rank);
      }
      g_king_attacks[sq] = g_gold_attacks[side::BLACK][sq]
        | g_gold_attacks[side::WHITE][sq];
    }
    auto bishop_offset = 0;
    auto rook_offset = 0;
    for (auto sq = 0; sq < square::SIZE; sq++) {
      //TODO　重複を消す
      //bishop
      g_bishop_offsets[sq] = bishop_offset;
      auto bits = bit::g_bishop_mask[sq].pop_cnt();
      int num = 1 << bits;
      for (auto index = 0; index < num; index++) {
        bit::Bitboard occ = index_to_occ(index, bits,
            bit::g_bishop_mask[sq]);
        assert(
            bishop_offset
            + occ_to_index(occ & bit::g_bishop_mask[sq],
              bit::g_bishop_mask[sq])
            < g_bishop_attacks.size());
        g_bishop_attacks[bishop_offset
          + occ_to_index(occ & bit::g_bishop_mask[sq],
              bit::g_bishop_mask[sq])] = calc_slider_att(sq, occ,
                true);
      }
      bishop_offset += num;
      //rook
      g_rook_offsets[sq] = rook_offset;
      bits = bit::g_rook_mask[sq].pop_cnt();
      num = 1 << bits;
      for (auto index = 0; index < num; index++) {
        bit::Bitboard occ = index_to_occ(index, bits, bit::g_rook_mask[sq]);
        assert(
            rook_offset
            + occ_to_index(occ & bit::g_rook_mask[sq],
              bit::g_rook_mask[sq])
            < g_rook_attacks.size());
        g_rook_attacks[rook_offset
          + occ_to_index(occ & bit::g_rook_mask[sq],
              bit::g_rook_mask[sq])] = calc_slider_att(sq, occ,
                false);
      }
      rook_offset += num;
    }
    //lance
    for (auto sq = 0u; sq < g_lance_attacks[0].size(); sq++) {
      for (auto index = 0u; index < g_lance_attacks[0][sq].size(); index++) {
        //black
        auto file = square::file(sq);
        auto rank = square::rank(sq);
        //black
        auto inc_r = -1;
        g_lance_attacks[side::BLACK][sq][index].init();
        for (auto r = rank + inc_r; square::is_valid_rank(r); r += inc_r) {
          const auto sq2 = square::make(file, r);
          g_lance_attacks[side::BLACK][sq][index] |= sq2;
          if ((index << 1) & (1 << r)) {
            break;
          }
        }
        //white
        inc_r = 1;
        g_lance_attacks[side::WHITE][sq][index].init();
        for (auto r = rank + inc_r; square::is_valid_rank(r); r += inc_r) {
          const auto sq2 = square::make(file, r);
          g_lance_attacks[side::WHITE][sq][index] |= sq2;
          if ((index << 1) & (1 << r)) {
            break;
          }
        }

      }
    }
    //between behind
    for (auto from = 0; from < square::SIZE; from++) {
      for (auto to = 0; to < square::SIZE; to++) {
        g_between[from][to] = between_debug(from, to);
        g_behind[from][to] = behind_debug(from, to);
      }
    }

    //check table
    for (auto sd = 0; sd < side::SIZE; sd++) {
      for (auto sq = 0; sq < square::SIZE; sq++) {
        g_gold_check_table[sd][sq] = gold_check_debug(sd, sq);
        g_silver_check_table[sd][sq] = silver_check_debug(sd, sq);
        g_knight_check_table[sd][sq] = knight_check_debug(sd, sq);
        g_lance_check_table[sd][sq] = lance_check_debug(sd, sq);
        g_pawn_check_table[sd][sq] = pawn_check_debug(sd, sq);
      }
    }


  }
  inline bit::Bitboard get_rook_attack(const int sq, const bit::Bitboard &occ) {
    return g_rook_attacks[g_rook_offsets[sq]
      + occ_to_index(occ & bit::g_rook_mask[sq], bit::g_rook_mask[sq])];
  }
  inline bit::Bitboard get_bishop_attack(const int sq, const bit::Bitboard &occ) {
    return g_bishop_attacks[g_bishop_offsets[sq]
      + occ_to_index(occ & bit::g_bishop_mask[sq], bit::g_bishop_mask[sq])];
  }
  inline bit::Bitboard get_lance_attack(const int sd, const int sq,
      const bit::Bitboard &occ) {
    auto index = (occ.p(bit::Bitboard::select(sq)) >> g_lance_shift[sq]) & 0x7f;
    return g_lance_attacks[sd][sq][index];
  }
  inline bit::Bitboard get_pawn_attack(const int sd, const int sq) {
    return g_pawn_attacks[sd][sq];
  }
  template<int sd> bit::Bitboard get_pawn_attack(const bit::Bitboard pawn) {
    return (sd == side::BLACK) ? pawn >> 1 : pawn << 1;
  }
  inline bit::Bitboard get_knight_attack(const int sd, const int sq) {
    return g_knight_attacks[sd][sq];
  }
  inline bit::Bitboard get_silver_attack(const int sd, const int sq) {
    return g_silver_attacks[sd][sq];
  }
  inline bit::Bitboard get_gold_attack(const int sd, const int sq) {
    return g_gold_attacks[sd][sq];
  }
  inline bit::Bitboard get_king_attack(const int sq) {
    return g_king_attacks[sq];
  }
  inline bit::Bitboard get_prook_attack(const int sq, const bit::Bitboard &occ) {
    return (get_rook_attack(sq, occ) | get_king_attack(sq));
  }
  inline bit::Bitboard get_pbishop_attack(const int sq,
      const bit::Bitboard &occ) {
    return (get_bishop_attack(sq, occ) | get_king_attack(sq));
  }
  inline bit::Bitboard get_plus_attack(const int sq) {
    return get_gold_attack(side::BLACK, sq) & get_gold_attack(side::WHITE, sq);
  }
  inline bit::Bitboard get_x_attack(const int sq) {
    return get_silver_attack(side::BLACK, sq)
      & get_silver_attack(side::WHITE, sq);
  }
  template<piece::Piece pc>
    bit::Bitboard attack_from(const int sd, const int sq,
        const bit::Bitboard & occ) {
      switch (pc) {
        case piece::PAWN:
          return get_pawn_attack(sd, sq);
        case piece::LANCE:
          return get_lance_attack(sd, sq, occ);
        case piece::KNIGHT:
          return get_knight_attack(sd, sq);
        case piece::SILVER:
          return get_silver_attack(sd, sq);
        case piece::GOLD:
        case piece::PPAWN:
        case piece::PLANCE:
        case piece::PKNIGHT:
        case piece::GOLDS:
        case piece::PSILVER:
          return get_gold_attack(sd, sq);
        case piece::KING:
          return get_king_attack(sq);
        case piece::BISHOP:
          return get_bishop_attack(sq, occ);
        case piece::ROOK:
          return get_rook_attack(sq, occ);
        case piece::PBISHOP:
          return get_pbishop_attack(sq, occ);
        case piece::PROOK:
          return get_prook_attack(sq, occ);
        default:
          assert(false);
      }
    }
  template<piece::Piece pc>
    bit::Bitboard pseudo_attack_from(const int sd, const int sq) {
      switch (pc) {
        case piece::PAWN:
          return get_pawn_attack(sd, sq);
        case piece::LANCE:
          return g_lance_attacks[sd][sq][0];
        case piece::KNIGHT:
          return get_knight_attack(sd, sq);
        case piece::SILVER:
          return get_silver_attack(sd, sq);
        case piece::GOLD:
        case piece::PPAWN:
        case piece::PLANCE:
        case piece::PKNIGHT:
        case piece::GOLDS:
        case piece::PSILVER:
          return get_gold_attack(sd, sq);
        case piece::KING:
          return get_king_attack(sq);
        case piece::BISHOP:
          return g_bishop_attacks[g_bishop_offsets[sq]];
        case piece::ROOK:
          return g_rook_attacks[g_rook_offsets[sq]];
        case piece::PBISHOP:
          return g_bishop_attacks[g_bishop_offsets[sq]] | get_king_attack(sq);
        case piece::PROOK:
          return g_rook_attacks[g_rook_offsets[sq]] | get_king_attack(sq);
        default:
          assert(false);
      }
    }
  bit::Bitboard pseudo_attack_from(const int pc, const int sd, const int sq) {
    switch (pc) {
      case piece::PAWN:
        return get_pawn_attack(sd, sq);
      case piece::LANCE:
        return g_lance_attacks[sd][sq][0];
      case piece::KNIGHT:
        return get_knight_attack(sd, sq);
      case piece::SILVER:
        return get_silver_attack(sd, sq);
      case piece::GOLD:
      case piece::PPAWN:
      case piece::PLANCE:
      case piece::PKNIGHT:
      case piece::GOLDS:
      case piece::PSILVER:
        return get_gold_attack(sd, sq);
      case piece::KING:
        return get_king_attack(sq);
      case piece::BISHOP:
        return g_bishop_attacks[g_bishop_offsets[sq]];
      case piece::ROOK:
        return g_rook_attacks[g_rook_offsets[sq]];
      case piece::PBISHOP:
        return g_bishop_attacks[g_bishop_offsets[sq]] | get_king_attack(sq);
      case piece::PROOK:
        return g_rook_attacks[g_rook_offsets[sq]] | get_king_attack(sq);
      default:
        assert(false);
        bit::Bitboard b;
        b.init();
        return b;
    }
  }
  inline bit::Bitboard ray(const int from, const int to) {
    return g_between[from][to] | g_behind[from][to];
  }
}

namespace engine {
  struct Engine {
    int hash;
    bool ponder;
    int threads;
    bool log;
  };
  Engine g_engine;
  void init() {
    g_engine.hash = 128;
    g_engine.ponder = false;
    g_engine.threads = 1;
    g_engine.log = false;
  }
}

namespace score {

  enum {
    NONE = -32767,
    MIN = -32700,
    EVAL_MIN = -30000,
    EVAL_MAX = +30000,
    MAX = +32700,
    MATE = +32000,
    SUPERIOR_MIN = -32100,
    SUPERIOR_MAX = +32100,
    REP_MIN = -31000,
    REP_MAX = 31000,
    FV_SCALE  = 32,
  };

  enum {
    FLAGS_NONE = 0,
    FLAGS_LOWER = 1 << 0,
    FLAGS_UPPER = 1 << 1,
    FLAGS_EXACT = FLAGS_LOWER | FLAGS_UPPER,
  };

  enum {
    REP_WIN,
    REP_LOSE,
    REP_EQUAL,
    REP_CHECK,
    REP_CHECKED,
    REP_UNKNOWN,
  };

  bool is_mate(const int sc) {
    return sc < EVAL_MIN || sc > EVAL_MAX;
  }

  int signed_mate(const int sc) {
    if (sc < EVAL_MIN) { // -MATE
      return -(MATE + sc) / 2;
    } else if (sc > EVAL_MAX) { // +MATE
      return (MATE - sc + 1) / 2;
    } else {
      assert(false);
      return 0;
    }
  }

  int side_score(const int sc, const int sd) {
    return (sd == side::BLACK) ? +sc : -sc;
  }

  int from_trans(const int sc, const int ply) {
    if (sc < EVAL_MIN) {
      return sc + ply;
    } else if (sc > EVAL_MAX) {
      return sc - ply;
    } else {
      return sc;
    }
  }

  int to_trans(const int sc, const int ply) {
    if (sc < EVAL_MIN) {
      return sc - ply;
    } else if (sc > EVAL_MAX) {
      return sc + ply;
    } else {
      return sc;
    }
  }

  int flags(const int sc, const int alpha, const int beta) {

    int flags = FLAGS_NONE;
    if (sc > alpha)
      flags |= FLAGS_LOWER;
    if (sc < beta)
      flags |= FLAGS_UPPER;

    return flags;
  }

}

namespace board {
  class Copy {
    public:
      hash_t key_;
      uint32 hand_b_;
      int flags_;
      int moves_;
      int recap_;
      bool in_checked_;
      void init() {
        key_ = 0;
        flags_ = 0;
        moves_ = 0;
        recap_ = 0;
        in_checked_ = false;
      }
  };
  class Undo {
    public:
      Copy copy_;
      int move_;
      int cap_sq_;
  };
  const std::string start_sfen =
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
  class Board {
    private:
      static const int SCORE_NONE = -10000; // HACK because "score::NONE" is defined later
      std::array<bit::Bitboard, piece::SIZE> piece_;
      std::array<bit::Bitboard, side::SIZE> side_;
      bit::Bitboard all_;
      std::array<int, side::SIZE> king_;
      std::array<int, piece::SIDE_SIZE> count_;
      std::array<int, square::SIZE> square_;
      std::array<uint32, side::SIZE> hand_;
      int turn_;
      Copy copy_;

      int root_;
      int sp_;
      std::array<Undo, 1024> stack_;
    private:
      template<bool update_copy> void clear_square(const int pc, const int sd,
          const int sq) {
        assert(piece::is_valid_piece(pc));
        assert(square::is_valid_sq(sq));
        assert(pc == square(sq));
        assert(piece_[pc].is_set(sq));
        piece_[pc].clear(sq);
        assert(side_[sd].is_set(sq));
        side_[sd].clear(sq);

        assert(square_[sq] != piece::NONE);
        square_[sq] = piece::NONE;
        const auto p32 = piece::make(pc, sd);
        assert(count_[p32] != 0);
        --count_[p32];
        if (update_copy) {
          copy_.key_ ^= hash::piece_key(p32, sq);
        }
      }
      template<bool update_copy> void set_square(const int pc, const int sd,
          const int sq) {
        assert(piece::is_valid_piece(pc));
        assert(square::is_valid_sq(sq));
        assert(!piece_[pc].is_set(sq));
        piece_[pc].set(sq);
        assert(!side_[sd].is_set(sq));
        side_[sd].set(sq);
        square_[sq] = pc;
        if (pc == piece::KING) {
          king_[sd] = sq;
        }
        const auto p32 = piece::make(pc, sd);
        ++count_[p32];
        if (update_copy) {
          copy_.key_ ^= hash::piece_key(p32, sq);
        }
      }
      void flip_turn() {
        turn_ = side::opposit(turn_);
        copy_.key_ ^= hash::turn_flip();
      }
      void update() {
        all_ = side_[side::BLACK] | side_[side::WHITE];
      }
    public:
      Board(){}
      Board(std::string s) {
        clear();
        init_sfen(s);
      }
      void operator=(const Board & bd) {
        std::copy(bd.piece_.begin(), bd.piece_.end(), piece_.begin());
        std::copy(bd.side_.begin(), bd.side_.end(), side_.begin());
        std::copy(bd.king_.begin(), bd.king_.end(), king_.begin());
        all_ = bd.all_;
        std::copy(bd.count_.begin(), bd.count_.end(), count_.begin());
        std::copy(bd.square_.begin(), bd.square_.end(), square_.begin());
        std::copy(bd.hand_.begin(), bd.hand_.end(), hand_.begin());
        turn_ = bd.turn_;
        copy_ = bd.copy_;
        root_ = bd.root_;
        sp_ = bd.sp_;
        std::copy(bd.stack_.begin(), bd.stack_.end(), stack_.begin());
      }
      bit::Bitboard piece(const int pc) const {
        assert(piece::is_valid_piece(pc));
        return piece_[pc];
      }
      bit::Bitboard piece(const int pc, const int sd) const {
        assert(piece::is_valid_piece(pc));
        return piece_[pc] & side_[sd];
      }
      bit::Bitboard golds() const {
        return (piece_[piece::GOLD] | piece_[piece::PPAWN]
            | piece_[piece::PLANCE] | piece_[piece::PKNIGHT]
            | piece_[piece::PSILVER]);
      }
      bit::Bitboard golds(const int sd) const {
        return golds() & side(sd);
      }
      int count(const int pc, const int sd) const {
        return count_[piece::make(pc, sd)];
      }
      bit::Bitboard side(const int sd) const {
        return side_[sd];
      }
      bit::Bitboard all() const {
        return all_;
      }
      int square(const int sq) const {
        return square_[sq];
      }
      int square_side(const int sq) const {
        assert(square(sq) != piece::NONE);
        return !(side_[side::BLACK].is_set(sq));
      }
      bool square_is(const int sq, const int pc, const int sd) const {
        assert(piece::is_valid_piece(pc));
        return (square(sq) == pc) && (square_side(sq) == sd);
      }
      int king(const int sd) const {
        return king_[sd];
      }
      int turn() const {
        return turn_;
      }
      hash_t key() const {
        return copy_.key_;
      }
      int ply() const {
        assert(sp_ >= root_);
        return sp_ - root_;
      }
      int last_move() const {
        return !(sp_) ? move::NONE : stack_[sp_ - 1].move_;
      }
      int is_draw() const {

        const auto key = copy_.key_; // HACK: ignores castling flags and e.p. square
        const auto hand_b = copy_.hand_b_;
        const auto in_checked = copy_.in_checked_;

        for (int i = sp_ - 4; i >= 0; i -= 2) {
          if (stack_[i].copy_.key_ == key) {
            if(stack_[i].copy_.hand_b_ == hand_b) {
              if(in_checked && stack_[i+2].copy_.in_checked_) {
                return score::REP_CHECKED;
              }
              if(stack_[i+3].copy_.in_checked_) {
                return score::REP_CHECK;
              }
              return score::REP_EQUAL;
            } else if(hand::is_superior(hand_b,stack_[i].copy_.hand_b_)) {
              return (turn() == side::BLACK) ? score::REP_WIN : score::REP_LOSE;
            } else if(hand::is_superior(stack_[i].copy_.hand_b_,hand_b)) {
              return (turn() == side::BLACK) ? score::REP_LOSE : score::REP_WIN;
            } else {
            }
          }
        }

        return score::REP_UNKNOWN;
      }
      void set_root() {
        root_ = sp_;
      }
      int sp() const {
        return sp_;
      }
      uint32 hand(const int sd) const {
        return hand_[sd];
      }
      void clear() {
        bit::Bitboard zero;
        zero.init();
        piece_.fill(zero);
        side_.fill(zero);
        square_.fill(0);
        king_.fill(0);
        count_.fill(0);
        hand_.fill(0);
        turn_ = side::BLACK;
        copy_.init();
        root_ = 0;
        sp_ = 0;
      }
      hash_t calc_key() const {
        hash_t key = 0;
        for (auto sq = 0; sq < square::SIZE; sq++) {
          if (all_.is_set(sq)) {
            const auto sd = square_side(sq);
            const auto pc = square(sq);
            const auto p32 = piece::make(pc, sd);
            key ^= hash::piece_key(p32, sq);
          }
        }
        key ^= hash::turn_key(turn_);
        return key;
      }
      bool is_ok() const {
        const auto side_all = side_[side::BLACK] | side_[side::WHITE];
        if (side_all != all_) {
          return false;
        }
        bit::Bitboard piece_all;
        piece_all.init();
        for (auto pc = int(piece::PAWN); pc < piece::GOLDS; pc++) {
          piece_all |= piece_[pc];
        }
        if (piece_all != all_) {
          return false;
        }
        for (auto pc = int(piece::PAWN); pc < piece::GOLDS; pc++) {
          bit::Bitboard piece = piece_[pc];
          while (!piece.is_empty()) {
            const auto sq = piece.lsb();
            if (square(sq) != pc) {
              return false;
            }
          }
        }
        for (auto sq = 0; sq < square::SIZE; sq++) {
          const auto pc = square(sq);
          if (!piece::is_valid(pc)) {
            return false;
          }
          if (pc == piece::NONE) {
            if (all_.is_set(sq)) {
              return false;
            }
          } else {
            if (!piece_[pc].is_set(sq)) {
              return false;
            }
          }
        }
        if (copy_.key_ != calc_key()) {
          return false;
        }
        if (copy_.hand_b_ != hand_[side::BLACK]) {
          return false;
        }
        for (auto file = 0; file < square::FILE_SIZE; file++) {
          bit::Bitboard b_pawn = piece(piece::PAWN, side::BLACK)
            & bit::g_file_mask[file];
          bit::Bitboard w_pawn = piece(piece::PAWN, side::WHITE)
            & bit::g_file_mask[file];
          if (b_pawn.pop_cnt() >= 2) {
            return false;
          }
          if (w_pawn.pop_cnt() >= 2) {
            return false;
          }
        }
        bit::Bitboard rank_1 = piece(piece::PAWN, side::BLACK)
          | piece(piece::LANCE, side::BLACK)
          | piece(piece::KNIGHT, side::BLACK);
        bit::Bitboard rank_9 = piece(piece::PAWN, side::WHITE)
          | piece(piece::LANCE, side::WHITE)
          | piece(piece::KNIGHT, side::WHITE);
        if (!(rank_1 & bit::g_rank_mask[square::RANK_1]).is_empty()) {
          return false;
        }
        if (!(rank_9 & bit::g_rank_mask[square::RANK_9]).is_empty()) {
          //std::cout<<rank_9<<std::endl;
          return false;
        }
        bit::Bitboard rank_2 = piece(piece::KNIGHT, side::BLACK);
        bit::Bitboard rank_8 = piece(piece::KNIGHT, side::WHITE);
        if (!(rank_2 & bit::g_rank_mask[square::RANK_2]).is_empty()) {
          return false;
        }
        if (!(rank_8 & bit::g_rank_mask[square::RANK_8]).is_empty()) {
          return false;
        }
        if (!piece(piece::KING, side::BLACK).is_set(king(side::BLACK))) {
          return false;
        }
        if (!piece(piece::KING, side::WHITE).is_set(king(side::WHITE))) {
          return false;
        }
        auto num = 0;
        num += all().pop_cnt();
        for (auto sd = 0; sd < side::SIZE; sd++) {
          num += hand::num(hand(sd), piece::PAWN);
          num += hand::num(hand(sd), piece::LANCE);
          num += hand::num(hand(sd), piece::KNIGHT);
          num += hand::num(hand(sd), piece::SILVER);
          num += hand::num(hand(sd), piece::GOLD);
          num += hand::num(hand(sd), piece::BISHOP);
          num += hand::num(hand(sd), piece::ROOK);
        }
        if (num != 40) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::PAWN) + hand::num(hand(side::WHITE), piece::PAWN);
        if (num < 0 || num > 18) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::LANCE) + hand::num(hand(side::WHITE), piece::LANCE);
        if (num < 0 || num > 4) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::KNIGHT) + hand::num(hand(side::WHITE), piece::KNIGHT);
        if (num < 0 || num > 4) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::SILVER) + hand::num(hand(side::WHITE), piece::SILVER);
        if (num < 0 || num > 4) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::GOLD) + hand::num(hand(side::WHITE), piece::GOLD);
        if (num < 0 || num > 4) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::BISHOP) + hand::num(hand(side::WHITE), piece::BISHOP);
        if (num < 0 || num > 2) {
          return false;
        }
        num = hand::num(hand(side::BLACK), piece::ROOK) + hand::num(hand(side::WHITE), piece::ROOK);
        if (num < 0 || num > 2) {
          return false;
        }

        return true;
      }
      void init_sfen(const std::string & s) {
        clear();
        auto pos = 0;

        //piece placement

        auto sq = 0;
        auto prom_flag = false;
        while (pos < int(s.size())) {

          auto c = s[pos++];

          if (false) {

          } else if (c == ' ') {
            break;
          } else if (c == '/') {
            continue;
          } else if (std::isdigit(c)) {
            sq += c - '0';
          } else if (c == '+') {
            prom_flag = true;
            continue;
          } else { // assume piece
            assert(square::is_valid_sq(sq));
            std::string pc_str = { c };
            auto p32 = piece::from_sfen(pc_str);
            if (prom_flag) {
              p32 = piece::prom(p32);
              prom_flag = false;
            }
            const auto pc = piece::piece(p32);
            const auto sd = piece::side(p32);
            set_square<true>(pc, sd, square::from_sfen(sq));
            sq++;
          }
        }

        //assert(sq == square::SIZE);

        //turn

        turn_ = side::BLACK;

        if (pos < int(s.size())) {
          auto c = s[pos++];
          if (c == 'w') {
            turn_ = side::WHITE;
          }
          if (pos < int(s.size())) {
            assert(s[pos] == ' ');
            pos++;
          }
        }

        //hand
        auto num = 0;
        while (pos < int(s.size())) {
          auto c = s[pos++];
          if (false) {

          } else if (c == ' ') {
            break;
          } else if (c == '-') {
            break;
          } else if (std::isdigit(c)) {
            num = (num) ? 10 * num + int(c - '0') : int(c - '0');
          } else {
            std::string pc_str = { c };
            const auto p32 = piece::from_sfen(pc_str);
            const auto pc = piece::piece(p32);
            const auto sd = piece::side(p32);
            num = (num) ? num : 1;
            for (auto i = 0; i < num; i++) {
              hand_[sd] = hand::change<true>(hand_[sd], pc);
            }
            num = 0;
          }
        }

        copy_.key_ ^= hash::turn_key(turn_);
        copy_.hand_b_ = hand_[side::BLACK];
        update();
        assert(is_ok());
      }
      std::string out_sfen() const {
        std::string str = "sfen ";
        auto empty = 0;
        for(auto rank = 0; rank < square::RANK_SIZE; rank++) {
          for(auto file = square::FILE_SIZE-1; file >= 0; file--) {
            const auto sq = square::make(file,rank);
            if(square(sq) == piece::NONE) {
              empty++;
              continue;
            } else {
              const auto sd = square_side(sq);
              const auto pc = square(sq);
              const auto p32 = piece::make(pc,sd);
              if(empty) {
                str += util::to_string(empty);
                empty = 0;
              }
              str += util::rtrim(piece::to_sfen(p32));
            }
          }
          if(empty) {
            str += util::rtrim(util::to_string(empty));
            empty = 0;
          }
          if(rank + 1 != square::RANK_SIZE) { str += "/"; }
        }
        str += (turn() == side::BLACK) ? " b " : " w ";
        if(hand(side::BLACK) != 0 || hand(side::WHITE) != 0) {
          for(auto sd = 0; sd < side::SIZE; sd++) {
            for(auto pc = int(piece::PAWN); pc <= piece::GOLD; pc++) {
              const auto num = hand::num(hand(sd),pc);
              if(num) {
                if(num > 1) {
                  str += util::to_string(num);
                }
                const auto p32 = piece::make(pc,sd);
                str += util::rtrim(piece::to_sfen(p32));
              }
            }
          }
          str += " ";
        }
        str += "- ";
        str += util::to_string(sp()+1);
        return str;
      }
      friend std::ostream& operator<<(std::ostream& os, const Board & b) {
        os << side::to_string(b.turn()) << std::endl;
        os << "key:" << b.key() << std::endl;
        os << "ply:" << b.ply() << std::endl;
        os <<b.out_sfen()<<std::endl;
        os << hand::to_string(b.hand(side::WHITE)) << std::endl;
        for (auto rank = 0; rank < square::RANK_SIZE; rank++) {
          if (!rank) {
            os << "  ";
            for (auto file = square::FILE_SIZE - 1; file >= 0; file--) {
              os <<" "<< file + 1 << " ";
            }
            os << "\n";
          }
          for (auto file = square::FILE_SIZE - 1; file >= 0; file--) {
            if (file == square::FILE_SIZE - 1) {
              os << char(rank + 'a') << ":";
            }
            const auto sq = square::make(file, rank);
            const auto pc = b.square(sq);
            if (pc == piece::NONE) {
              os << piece::to_str(pc);
            } else {
              const auto sd = b.square_side(sq);
              const auto p32 = piece::make(pc, sd);
              os << piece::to_str(p32);
            }
          }
          os << "\n";
        }
        os << hand::to_string(b.hand(side::BLACK)) << std::endl;
        return os;
      }
      void move(const int mv, const bool check) {
        assert(is_ok());
        assert(mv != move::NONE);
        assert(mv != move::NULL_);
        const auto sd = turn();
        const auto xd = side::opposit(sd);

        const auto f = move::from(mv);
        const auto t = move::to(mv);

        auto pc = move::piece(mv);
        auto cp = move::cap(mv);
        const auto prom = move::prom(mv);

        assert(sp_ < int(stack_.size()));
        Undo & undo = stack_[sp_++];
        undo.copy_ = copy_;
        undo.move_ = mv;
        copy_.recap_ = square::NONE;

        if (move::is_drop(mv)) {
          set_square<true>(pc, sd, t);
          hand_[sd] = hand::change<false>(hand_[sd], pc);
        } else {
          assert(square(f) == pc);
          assert(sd == square_side(f));
          assert(cp != piece::KING);
          //capture
          if (cp) {
            assert(square(t) == cp);
            assert(square_side(t) == xd);
            undo.cap_sq_ = t;
            clear_square<true>(cp, xd, t);
            cp = piece::unprom(cp);
            hand_[sd] = hand::change<true>(hand_[sd], cp);
          }
          //promotion
          if (prom) {
            clear_square<true>(pc, sd, f);
            pc = piece::prom(pc);
            set_square<true>(pc, sd, t);
          } else {
            clear_square<true>(pc, sd, f);
            set_square<true>(pc, sd, t);
          }
        }
        flip_turn();
        copy_.hand_b_ = hand_[side::BLACK];
        copy_.in_checked_ = check;
        update();
        assert(is_ok());
      }
      void undo() {
        assert(is_ok());
        assert(sp_ > 0);
        const Undo & undo = stack_[--sp_];
        const int mv = undo.move_;
        const auto f = move::from(mv);
        const auto t = move::to(mv);
        auto pc = move::piece(mv);
        auto cp = move::cap(mv);
        const auto prom = move::prom(mv);

        const int xd = turn();
        const int sd = side::opposit(xd);

        if (move::is_drop(mv)) {
          clear_square<false>(pc, sd, t);
          hand_[sd] = hand::change<true>(hand_[sd], pc);
        } else {
          //promotion
          if (prom) {
            const auto pp = piece::prom(pc);
            assert(pp == square(t));
            clear_square<false>(pp, sd, t);
            set_square<false>(pc, sd, f);
          } else {
            clear_square<false>(pc, sd, t);
            set_square<false>(pc, sd, f);
          }
          if (cp) {
            set_square<false>(cp, xd, t);
            cp = piece::unprom(cp);
            hand_[sd] = hand::change<false>(hand_[sd], cp);
          }
        }
        flip_turn();
        copy_ = undo.copy_;
        update();
        assert(is_ok());
      }

      void move_null() {
        assert(sp_ < 1024);
        Undo & undo = stack_[sp_++];
        undo.move_ = move::NULL_;

        undo.copy_ = copy_;

        flip_turn();
        copy_.moves_ = 0;
        copy_.recap_ = square::NONE;
        //update();
      }

      void undo_null() {
        assert(sp_ > 0);
        const Undo & undo = stack_[--sp_];
        assert(undo.move_ == move::NULL_);

        flip_turn();
        copy_ = undo.copy_;
        //update();
      }

      void set_square(const int pc, const int sd, const int sq) {
        assert(piece::is_valid_piece(pc));
        assert(square::is_valid_sq(sq));
        piece_[pc].set(sq);
        side_[sd].set(sq);
        all_.set(sq);
      }
      void clear_square(const int pc, const int sd, const int sq) {
        assert(piece::is_valid_piece(pc));
        assert(square::is_valid_sq(sq));
        piece_[pc].clear(sq);
        side_[sd].clear(sq);
        all_.clear(sq);
      }
  };
}
namespace attack {

  template<bool skip_pawn, bool skip_king>
    bit::Bitboard attack_to(const int sd, const int sq, const board::Board & bd) {

      const auto xd = side::opposit(sd);
      bit::Bitboard b;
      b.init();
      if (!skip_pawn) {
        b |= bd.piece(piece::PAWN, sd)
          & attack::attack_from<piece::PAWN>(xd, sq, bd.all());
      }
      b |= bd.piece(piece::LANCE, sd)
        & attack::attack_from<piece::LANCE>(xd, sq, bd.all());
      b |= bd.piece(piece::KNIGHT, sd)
        & attack::attack_from<piece::KNIGHT>(xd, sq, bd.all());
      b |= bd.piece(piece::SILVER, sd)
        & attack::attack_from<piece::SILVER>(xd, sq, bd.all());
      b |= bd.golds(sd) & attack::attack_from<piece::GOLD>(xd, sq, bd.all());
      if(skip_king) {
        b |= (bd.piece(piece::PBISHOP,sd) | bd.piece(piece::PROOK,sd))
          & attack::attack_from<piece::KING>(xd, sq, bd.all());
      } else {
        b |= (bd.piece(piece::KING, sd) | bd.piece(piece::PBISHOP,sd)
            | bd.piece(piece::PROOK,sd))
          & attack::attack_from<piece::KING>(xd, sq, bd.all());
      }

      b |= (bd.piece(piece::BISHOP, sd) | bd.piece(piece::PBISHOP,sd))
        & attack::attack_from<piece::BISHOP>(xd, sq, bd.all());
      b |= (bd.piece(piece::ROOK, sd) | bd.piece(piece::PROOK,sd))
        & attack::attack_from<piece::ROOK>(xd, sq, bd.all());
      return b;
    }

  void init_attacks(Attacks & attacks, const board::Board & bd) {
    const auto sd = bd.turn();
    const auto atk = side::opposit(sd);
    const auto def = sd;
    const auto t = bd.king(def);

    attacks.size = 0;
    attacks.avoid.init();
    attacks.pinned.init();

    // non-sliders
    {
      bit::Bitboard b;
      b.init();
      b |= bd.piece(piece::PAWN, atk) & attack::get_pawn_attack(def, t);
      b |= bd.piece(piece::KNIGHT, atk) & attack::get_knight_attack(def, t);
      b |= bd.piece(piece::SILVER, atk) & attack::get_silver_attack(def, t);
      b |= bd.golds(atk) & attack::get_gold_attack(def, t);
      bit::Bitboard x = bd.piece(piece::KING, atk)
        | bd.piece(piece::PBISHOP, atk);
      bit::Bitboard p = bd.piece(piece::KING, atk)
        | bd.piece(piece::PROOK, atk);
      b |= x & attack::get_plus_attack(t);
      b |= p & attack::get_x_attack(t);

      if (!b.is_empty()) {
        attacks.square[attacks.size++] = b.lsb<false>();
      }
    }
    // silders
    {
      bit::Bitboard b;
      b.init();
      b |= bd.piece(piece::LANCE, atk)
        & attack::pseudo_attack_from<piece::LANCE>(sd, t);
      b |= (bd.piece(piece::BISHOP, atk) | bd.piece(piece::PBISHOP, atk))
        & attack::pseudo_attack_from<piece::BISHOP>(sd, t);
      b |= (bd.piece(piece::ROOK, atk) | bd.piece(piece::PROOK, atk))
        & attack::pseudo_attack_from<piece::ROOK>(sd, t);
      while (!b.is_empty()) {
        const auto f = b.lsb();
        bit::Bitboard bb = bd.all() & g_between[f][t];
        if (bb.is_empty()) {
          attacks.square[attacks.size++] = f;
          attacks.avoid |= ray(f, t);
        } else if (bb.pop_cnt() == 1) {
          attacks.pinned |= bb;
        }
      }
    }
    assert(attacks.size <= 2);
  }
  bit::Bitboard pinned_by(const int sd, const board::Board & bd) {

    bit::Bitboard pinned;
    bit::Bitboard b;

    const auto atk = side::opposit(sd);
    const auto def = sd;
    const auto t = bd.king(def);

    pinned.init();
    b.init();
    b |= bd.piece(piece::LANCE, atk)
      & attack::pseudo_attack_from<piece::LANCE>(sd, t);
    b |= (bd.piece(piece::BISHOP, atk) | bd.piece(piece::PBISHOP, atk))
      & attack::pseudo_attack_from<piece::BISHOP>(sd, t);
    b |= (bd.piece(piece::ROOK, atk) | bd.piece(piece::PROOK, atk))
      & attack::pseudo_attack_from<piece::ROOK>(sd, t);
    while (!b.is_empty()) {
      const auto f = b.lsb();
      bit::Bitboard bb = bd.all() & g_between[f][t];
      if (bb.pop_cnt() == 1) {;
        pinned |= bb;
      }
    }
    return pinned;
  }
  bool is_attacked(const int t, const int sd, const board::Board & bd,
      const bit::Bitboard & occ) {
    const auto xd = side::opposit(sd);
    // non-sliders
    if (!(bd.piece(piece::PAWN, sd) & attack::get_pawn_attack(xd, t)).is_empty()) {
      return true;
    }
    if (!(bd.piece(piece::KNIGHT, sd) & attack::get_knight_attack(xd, t)).is_empty()) {
      return true;
    }
    if (!(bd.piece(piece::SILVER, sd) & attack::get_silver_attack(xd, t)).is_empty()) {
      return true;
    }
    if (!(bd.golds(sd) & attack::get_gold_attack(xd, t)).is_empty()) {
      return true;
    }
    if (!((bd.piece(piece::KING, sd) | bd.piece(piece::PBISHOP, sd)
            | bd.piece(piece::PROOK, sd)) & attack::get_king_attack(t)).is_empty()) {
      return true;
    }
    //silders

    if (!((bd.piece(piece::ROOK, sd) | bd.piece(piece::PROOK, sd))
          & attack::get_rook_attack(t, occ)).is_empty()) {
      return true;
    }
    if (!((bd.piece(piece::BISHOP, sd) | bd.piece(piece::PBISHOP, sd))
          & attack::get_bishop_attack(t, occ)).is_empty()) {
      return true;
    }
    if (!(bd.piece(piece::LANCE, sd) & attack::get_lance_attack(xd, t, occ)).is_empty()) {
      return true;
    }
    return false;
  }
  bool is_attacked(const int t, const int sd, const board::Board & bd) {
    return is_attacked(t, sd, bd, bd.all());
  }
  bool is_legal(const board::Board & bd) {
    const auto atk = bd.turn();
    const auto def = side::opposit(atk);
    return !is_attacked(bd.king(def), atk, bd);
  }
  bool is_in_check(const board::Board & bd) {
    const auto atk = bd.turn();
    const auto def = side::opposit(atk);
    return is_attacked(bd.king(atk), def, bd);
  }

  bool attack(const int pc, const int sd, const int f, const int t, const board::Board & bd) {
    switch(pc) {
      case piece::PAWN:
        return pseudo_attack_from<piece::PAWN>(sd,f).is_set(t);
      case piece::KNIGHT:
        return pseudo_attack_from<piece::KNIGHT>(sd,f).is_set(t);
      case piece::SILVER:
        return pseudo_attack_from<piece::SILVER>(sd,f).is_set(t);
      case piece::GOLD:
      case piece::PPAWN:
      case piece::PLANCE:
      case piece::PKNIGHT:
      case piece::PSILVER:
        return pseudo_attack_from<piece::GOLD>(sd,f).is_set(t);
      case piece::KING:
        return false;
        //これはダメな気がする
        //return pseudo_attack_from<piece::GOLD>(sd,f).is_set(t);
      case piece::LANCE:
        return pseudo_attack_from<piece::LANCE>(sd,f).is_set(t)
          && (attack::g_between[f][t] & bd.all()).is_empty();
      case piece::BISHOP:
        return pseudo_attack_from<piece::BISHOP>(sd,f).is_set(t)
          && (attack::g_between[f][t] & bd.all()).is_empty();
      case piece::PBISHOP:
        return pseudo_attack_from<piece::PBISHOP>(sd,f).is_set(t)
          && (attack::g_between[f][t] & bd.all()).is_empty();
      case piece::ROOK:
        return pseudo_attack_from<piece::ROOK>(sd,f).is_set(t)
          && (attack::g_between[f][t] & bd.all()).is_empty();
      case piece::PROOK:
        return pseudo_attack_from<piece::PROOK>(sd,f).is_set(t)
          && (attack::g_between[f][t] & bd.all()).is_empty();
      default: assert(false); return false;
    }
  }

  bool attack_behind(const int f, const int t, const int sd, const board::Board & bd) {

    assert(bd.square(t) != piece::NONE);

    bit::Bitboard behind = attack::g_behind[f][t];

    if (behind.is_empty()) return false;
    bit::Bitboard b;
    const auto xd = side::opposit(sd);
    b  = bd.piece(piece::LANCE,sd) & pseudo_attack_from<piece::LANCE>(xd,t);
    b |= (bd.piece(piece::ROOK,sd)   | bd.piece(piece::PROOK,sd))   & pseudo_attack_from<piece::ROOK>(xd,t);
    b |= (bd.piece(piece::BISHOP,sd) | bd.piece(piece::PBISHOP,sd)) & pseudo_attack_from<piece::BISHOP>(xd,t);
    b &= behind;

    while (!b.is_empty()) {

      const auto sq = b.lsb();

      if ((bd.all() & g_between[sq][f]).pop_cnt() == 1) {
        return true;
      }
    }

    return false;
  }

}

namespace move {
  constexpr int next_piece(const int pc) {
    return (pc == piece::PAWN)    ? piece::LANCE :
      (pc == piece::LANCE)   ? piece::KNIGHT :
      (pc == piece::KNIGHT)  ? piece::SILVER :
      (pc == piece::SILVER)  ? piece::GOLDS :
      (pc == piece::GOLDS)   ? piece::BISHOP :
      (pc == piece::BISHOP)  ? piece::PBISHOP :
      (pc == piece::PBISHOP) ? piece::ROOK :
      (pc == piece::ROOK)    ? piece::PROOK :
      (pc == piece::PROOK)   ? piece::KING
      : piece::KING;
  }
  class SEE {
    private:
      const board::Board  * board_;
      int to_;
      bit::Bitboard all_;
      int val_;

      void init(const int t) {
        to_ = t;
        all_ = board_->all();
        const auto pc = board_->square(t);
        val_ = piece::ex_value(pc);
      }
      template<int sd>int move(const int f) {
        all_.clear(f);
        const auto pc = board_->square(f);
        assert(piece::is_valid_piece(pc));
        assert(board_->square_side(f) == sd);
        auto val = val_;
        val_ = piece::ex_value(pc);
        if(piece::can_prom(pc)
            && (square::is_promotion<sd>(f) || square::is_promotion<sd>(to_))) {
          //util::Tee<<"prom:"<<piece::prom_value(pc)<<std::endl;
          val  += piece::prom_value(pc);
          val_ += piece::prom_value(pc);
        }
        if(val == piece::KING_VALUE) {
          all_.init();
        }
        //util::Tee<<"val:"<<val_<<std::endl;
        //util::Tee<<"ret:"<<val<<std::endl;
        return val;
      }
      template<int sd>int see_rec(int alpha, int beta) {
        assert(alpha < beta);
        auto s0 = 0;
        if(s0 > alpha) {
          alpha = s0;
          if(s0 >= beta) {
            return s0;
          }
        }
        if(val_ <= alpha) {
          return val_;
        }
        auto f = pick_lva<piece::PAWN>(sd);
        //std::cout<<piece::to_char(board_->square(f))<<std::endl;
        //std::cout<<val_<<std::endl;
        if(f == square::NONE) {
          return s0;
        }
        auto cap = move<sd>(f);
        auto s1  = cap - see_rec<side::opposit(sd)>(cap-beta,cap-alpha);
        return std::max(s0,s1);
      }
      template<int pc>int pick_lva(const int sd) {
        assert((piece::is_valid_piece(pc) || pc == piece::GOLDS));
        const int xd = side::opposit(sd);
        bit::Bitboard piece = (pc == piece::GOLDS) ? board_->golds(sd) : board_->piece(pc,sd);
        for(bit::Bitboard fs = piece & attack::pseudo_attack_from<piece::Piece(pc)>(xd,to_) & all_;
            !fs.is_empty();) {
          const auto f = fs.lsb();
          if((all_ & attack::g_between[f][to_]).is_empty()) {
            return f;
          }
        }
        return pick_lva<next_piece(pc)>(sd);
      }
    public:
      template<int sd>int see(int mv, int alpha, int beta, const board::Board & bd) {

        assert(alpha < beta);

        board_ = &bd;

        int f  = from(mv);
        int t  = to(mv);

        init(t);
        int cap;
        if(move::is_drop(mv)) {
          cap = piece::NONE;
          val_ = 0;
        } else {
          cap = move<sd>(f); // NOTE: assumes queen promotion
        }
        //		if (pc == piece::PAWN && square::is_promotion(t)) { // adjust for under-promotion
        //			int delta = piece::QUEEN_VALUE - piece::value(prom(mv));
        //			cap -= delta;
        //			val_ -= delta;
        //		}

        return cap - see_rec<side::opposit(sd)>(cap - beta, cap - alpha);
      }
  };
  //http://d.hatena.ne.jp/aki-yam/20120304/1330858526
  //外出ししないとダメらしい
  template<> int SEE::pick_lva<piece::KING>(const int sd) {
    const auto xd = side::opposit(sd);
    bit::Bitboard fs = attack::pseudo_attack_from<piece::KING>(xd,to_) & all_;
    if(fs.is_set(board_->king(sd))) {
      return board_->king(sd);
    } else {
      return square::NONE;
    }
  }

  int from_string(const std::string & s, const board::Board & bd) {
    if (std::isdigit(s[0])) { //move
      const auto from = square::from_string(s.substr(0, 2));
      const auto to = square::from_string(s.substr(2, 2));
      const auto pc = bd.square(from);
      const auto cp = bd.square(to);
      const auto prom = s.find("+") != std::string::npos;
      return move::make(from, to, pc, cp, prom);
    } else { //drop
      const auto pc = piece::from_string(s.substr(0, 1));
      const auto to = square::from_string(s.substr(2,2));
      return move::make(to, pc);
    }
  }

  bool is_check_debug(const int mv, board::Board & bd) {

    bd.move(mv,false);
    const auto b = attack::is_in_check(bd);
    bd.undo();
    return b;
  }

  bool is_direck_check(const int mv, board::Board & bd) {

    const auto t = to(mv);
    const auto pc = (prom(mv)) ? piece::prom(piece(mv)) : piece(mv);
    const auto sd = bd.turn();
    const auto king = bd.king(side::opposit(sd));
    //direct attack
    if(attack::attack(pc,sd,t,king,bd)) {
#ifdef DEBUG
      bool check_debug = move::is_check_debug(mv,bd);
      if(!check_debug) {
        std::cout<<bd<<std::endl;
        std::cout<<mv<<std::endl;
      }
      assert(check_debug);
#endif
      return true;
    }
    return false;
  }

  bool is_check(const int mv, board::Board & bd) {
    const auto f = from(mv);
    const auto t = to(mv);
    const auto pc = (prom(mv)) ? piece::prom(piece(mv)) : piece(mv);
    const auto sd = bd.turn();
    const auto king = bd.king(side::opposit(sd));
    //direct attack
    if(attack::attack(pc,sd,t,king,bd)) {
#ifdef DEBUG
      bool check_debug = move::is_check_debug(mv,bd);
      if(!check_debug) {
        std::cout<<bd<<std::endl;
        std::cout<<mv<<std::endl;
      }
      assert(check_debug);
#endif
      return true;
    }
    //discover attack
    if( !is_drop(mv)
        && attack::attack_behind(king,f,sd,bd)
        && !attack::ray(king,f).is_set(t)) {
#ifdef DEBUG
      bool check_debug = move::is_check_debug(mv,bd);
      if(!check_debug) {
        std::cout<<bd<<std::endl;
        std::cout<<mv<<std::endl;
      }
      assert(check_debug);
#endif
      return true;
    }
#ifdef DEBUG
    bool check_debug = move::is_check_debug(mv,bd);
    if(check_debug) {
      std::cout<<bd<<std::endl;
      std::cout<<mv<<std::endl;
    }
    assert(!check_debug);
#endif
    return false;
  }

  bool is_legal_debug(const int mv, board::Board & bd) {

    bd.move(mv,move::is_check(mv,bd));
    const auto b = attack::is_legal(bd);
    bd.undo();
    return b;
  }
  bool is_discover(const int f, const int t, const int king,
      const bit::Bitboard & pinned) {
    if (!pinned.is_set(f)) {
      return false;
    }
    if (attack::ray(king, f).is_set(t)) {
      return false;
    }
    return true;
  }
  bool is_legal(const int f, const int t, const board::Board & bd,
      const attack::Attacks & attacks) {

    const auto sd = bd.turn();

    if (bd.square(f) == piece::KING) {
      return !attack::is_attacked(t, side::opposit(sd), bd);
    } else {
      return !is_discover(f, t, bd.king(sd), attacks.pinned);
    }
  }
  bool is_legal(const int mv, const board::Board & bd,
      const attack::Attacks & attacks) {
    if (is_drop(mv)) {
      return true;
    } else {
      const auto f = from(mv);
      const auto t = to(mv);
      return is_legal(f, t, bd, attacks);
    }
  }

  template<int sd>int see(const int mv, const int alpha, const int beta, const board::Board & bd) {
    SEE see;
    return see.see<sd>(mv,alpha,beta,bd);
  }
  int see(int mv, int alpha, int beta, const board::Board & bd) {
    return bd.turn() == side::BLACK ? see<side::BLACK>(mv,alpha,beta,bd)
      : see<side::WHITE>(mv,alpha,beta,bd);
  }
  bool is_safe(const int mv, const board::Board & bd) {

    const auto pc = piece(mv);
    const auto cp = cap(mv);

    if (false) {
    } else if (pc == piece::KING) {
      return true;
    } else if (piece::value(cp) >= piece::value(pc)) {
      return true;
    } else {
      return see(mv, -1, 0, bd) >= 0;
    }
  }

  bool is_tactical(const int mv) {
    return (move::cap(mv))
      || (move::prom(mv) && move::piece(mv) == piece::PAWN);
  }

  bool is_capture(const int mv) {
    return (move::cap(mv) != piece::NONE);
  }

  bool is_win(int mv, const board::Board & bd) {

    assert(is_tactical(mv));

    const auto pc = piece(mv);
    const auto cp = cap(mv);

    if (false) {
    } else if (pc == piece::KING) {
      return true;
    } else if (piece::value(cp) > piece::value(pc)) {
      return true;
    } else {
      return see(mv, 0, +1, bd) > 0;
    }
  }

  int see_max(const int mv) {

    assert(is_tactical(mv));

    auto sc = piece::ex_value(cap(mv));

    const auto pp = prom(mv);
    if (pp) sc += piece::prom_value(pp);

    return sc;
  }

  int move_to_move16(const int mv) {
    if(move::is_drop(mv)) {
      const auto pc = move::piece(mv) + square::SIZE;
      const auto to = move::to(mv);
      assert(pc != piece::NONE + square::SIZE);
      assert(square::is_valid_sq(to));
      const auto mv = move::make(pc,to,0,0);
      assert(move::from(mv) != square::SIZE);
      return mv;
    } else {
      return mv & MOVE16_MASK;
    }
  }

  int move16_to_move(const int mv16, const board::Board & bd) {
    const auto to = move::to(mv16);
    if(move::is_drop(mv16)) {
      const auto pc = move::from(mv16) - square::SIZE;
      assert(pc != piece::NONE);
      const auto mv = move::make(to,pc);
      return mv;
    } else {
      const auto from = move::from(mv16);
      const auto pc = bd.square(from);
      const auto cp = bd.square(to);
      const auto prom = move::prom(mv16);
      return move::make(from,to,pc,cp,prom);
    }
  }

}

namespace attack {

  bool is_mate_with_pawn_drop(const int to, const board::Board & bd) {

    const auto sd = bd.turn();
    const auto xd = side::opposit(sd);

#ifdef DEBUG
    auto sq = 0;
    if (bd.turn() == side::BLACK) {
      sq = to + square::INC_UP;
    } else {
      sq = to + square::INC_DOWN;
    }
    assert(sq == bd.king(xd));
#endif

    bit::Bitboard piece = attack::attack_to<true,true>(xd, to, bd);
    const auto king_sq = bd.king(xd);
    const bit::Bitboard pinned = attack::pinned_by(xd, bd);
    //capture checker
    for (; !piece.is_empty();) {
      const auto from = piece.lsb();
      if (!move::is_discover(from, to, king_sq, pinned)) {
        return false;
      }
    }
    //escape king
    bit::Bitboard dist = attack::get_king_attack(king_sq) & (~bd.side(xd));
    bit::Bitboard occ = bd.all();
    occ.set(to);
    for (; !dist.is_empty();) {
      const auto sq = dist.lsb();
      if (!attack::is_attacked(sq, sd, bd, occ)) {
        return false;
      }
    }
    return true;
  }

}

namespace gen {

  enum MoveType {
    TACTICAL, QUIET, DROP, EVASION,
  };

  class List {
    private:
      static constexpr int SIZE = 600;
      uint64 * p_;
      uint64 pair_[SIZE];

      void move_to(const int pf, const int pt) {

        assert(pt <= pf);
        assert(&pair_[pf] < p_);

        auto p = pair_[pf];

        for (auto i = pf; i > pt; i--) {
          pair_[i] = pair_[i - 1];
        }

        pair_[pt] = p;
      }

      void add_pair(const uint64 p) {
        assert(p_ < &pair_[SIZE]);
        *p_++ = p;
      }

      uint64 pair(const int pos) const {
        assert(&pair_[pos] < p_);
        return pair_[pos];
      }

    public:

      List() {
        clear();
      }

      void operator=(const List & ml) {

        clear();

        for (auto pos = 0; pos < ml.size(); ++pos) {
          const auto p = ml.pair(pos);
          add_pair(p);
        }
      }

      void clear() {
        p_ = pair_;
      }

      void add(const int mv, const int sc = 0) {
        assert(mv >= 0 && mv < move::SIZE);
        assert(sc >= 0 && sc < (move::SCORE_SIZE));
        assert(!contain(mv));
        add_pair((sc << move::BITS) | mv);
      }

      void set_score(const int pos, const int sc) {
        assert(pos < size());
        assert(sc >= 0 && sc < (move::SCORE_SIZE));
        pair_[pos] = (uint64(sc) << move::BITS) | move(pos);
        assert(score(pos) == sc);
      }

      void move_to_front(const int pos) {
        move_to(pos, 0);
      }

      void sort() { // insertion sort

        for (int i = 1; i < size(); i++) {

          auto p = pair_[i];

          int j;

          for (j = i; j > 0 && pair_[j - 1] < p; j--) {
            pair_[j] = pair_[j - 1];
          }

          pair_[j] = p;
        }

        for (auto i = 0; i < size() - 1; i++) {
          assert(pair_[i] >= pair_[i + 1]);
        }
      }

      int size() const {
        return (p_ - pair_);
      }

      static int move(const uint64 pair) {
        return pair & move::MASK;
      }

      int move(const int pos) const {
        return List::move(pair(pos));
      }

      int score(const int pos) const {
        return pair(pos) >> move::BITS;
      }

      uint64* begin() {
        return pair_;
      }

      uint64* end() {
        return p_;
      }
      bool contain(const int mv) const {

        for (auto pos = 0; pos < size(); ++pos) {
          if (move(pos) == mv) {
            return true;
          }
        }

        return false;
      }
      bool contain_bona(const int mv) const {

        for (auto pos = 0; pos < size(); ++pos) {
          const auto from = move::from(mv);
          const auto to   = move::to(mv);
          const auto pc   = move::piece(mv);
          const auto cp   = move::cap(mv);

          const auto mv2   = move(pos);
          const auto from2 = move::from(mv2);
          const auto to2   = move::to(mv2);
          const auto pc2   = move::piece(mv2);
          const auto cp2   = move::cap(mv2);

          if (from == from2 && to == to2 && pc == pc2 && cp == cp2) {
            return true;
          }
        }

        return false;
      }
      int find(const int mv) const {

        for (auto i = 0; i < size(); ++i) {
          if (move(i) == mv) {
            return i;
          }
        }
        return -1;
      }
      std::string to_can() const {
        std::string str = "";
        for(auto i = 0; i < size(); ++i) {
          str += move::to_can(move(i)) + " : " + util::to_string(score(i)) + "\n";
        }
        return str;
      }
      std::string to_can2() const {
        std::string str = "";
        for(auto i = 0; i < size(); ++i) {
          str += move::to_can2(move(i)) + " : " + util::to_string(score(i)) + "\n";
        }
        return str;
      }
  };

  template<piece::Piece pc, MoveType mt, int sd>
    void add_noprom_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {
      bit::Bitboard piece =
        (pc == piece::GOLDS) ? bd.golds(sd) : bd.piece(pc, sd);
      for (; !piece.is_empty();) {
        const auto from = piece.lsb();
        for (bit::Bitboard att = attack::attack_from<pc>(sd, from, bd.all())
            & target; !att.is_empty();) {
          const auto to = att.lsb();
          ml.add(move::make(from, to, bd.square(from), bd.square(to)));
        }
      }

    }
  template<MoveType mt, int sd>
    void add_king_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {
      const auto from = bd.king(sd);
      for (bit::Bitboard att = attack::get_king_attack(from) & target;
          !att.is_empty();) {
        const auto to = att.lsb();
        ml.add(move::make(from, to, piece::KING, bd.square(to)));
      }
    }
  template<MoveType mt, int sd>
    void add_pawn_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {

      for (bit::Bitboard att = attack::get_pawn_attack<sd>(
            bd.piece(piece::PAWN, sd)) & target; !att.is_empty();) {
        const auto to = att.lsb();
        const auto from =
          (sd == side::BLACK) ?
          to + square::INC_DOWN : to + square::INC_UP;
        ml.add(
            move::make(from, to, piece::PAWN, bd.square(to),
              square::is_promotion<sd>(to)));
      }
    }
  template<piece::Piece pc, MoveType mt, int sd>
    void add_bishop_rook_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {
      static_assert(pc == piece::BISHOP || pc == piece::ROOK,"pc error");
      for (bit::Bitboard piece = bd.piece(pc, sd); !piece.is_empty();) {
        const auto from = piece.lsb();
        for (bit::Bitboard att = attack::attack_from<pc>(sd, from, bd.all())
            & target; !att.is_empty();) {
          const auto to = att.lsb();
          const auto prom = square::is_promotion<sd>(from)
            || square::is_promotion<sd>(to);
          ml.add(move::make(from, to, bd.square(from), bd.square(to), prom));
        }
      }
    }
  template<MoveType mt, int sd>
    void add_silver_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {
      for (bit::Bitboard piece = bd.piece(piece::SILVER, sd); !piece.is_empty();
          ) {
        const auto from = piece.lsb();
        for (bit::Bitboard att = attack::attack_from<piece::SILVER>(sd, from,
              bd.all()) & target; !att.is_empty();) {
          const auto to = att.lsb();
          const auto prom = square::is_promotion<sd>(from)
            || square::is_promotion<sd>(to);
          if (prom) {
            ml.add(
                move::make(from, to, piece::SILVER, bd.square(to),
                  true));
          }
          ml.add(move::make(from, to, piece::SILVER, bd.square(to), false));
        }
      }
    }
  template<piece::Piece pc, MoveType mt, int sd>
    void add_lance_knight_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {
      static_assert(pc == piece::LANCE || pc == piece::KNIGHT,"pc error");
      for (bit::Bitboard piece = bd.piece(pc, sd); !piece.is_empty();) {
        const auto from = piece.lsb();
        for (bit::Bitboard att = attack::attack_from<pc>(sd, from, bd.all())
            & target; !att.is_empty();) {
          const auto to = att.lsb();
          const auto to_rank = square::rank<sd>(to);
          if (to_rank == square::RANK_1 || to_rank == square::RANK_2) {
            ml.add(
                move::make(from, to, bd.square(from), bd.square(to),
                  true));
          } else if (to_rank == square::RANK_3) {
            ml.add(
                move::make(from, to, bd.square(from), bd.square(to),
                  true));
            ml.add(
                move::make(from, to, bd.square(from), bd.square(to),
                  false));
          } else {
            ml.add(
                move::make(from, to, bd.square(from), bd.square(to),
                  false));
          }
        }
      }
    }
  template<bool has_knight, bool has_lance, bool has_pawn, int sd, int num>
    void add_drop_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target, const int p[]) {

      if (has_pawn) {
        //pawn
        const bit::Bitboard mask = bit::g_rank_mask[square::RANK_1]
          | bit::g_rank_mask[square::RANK_2]
          | bit::g_rank_mask[square::RANK_3]
          | bit::g_rank_mask[square::RANK_4]
          | bit::g_rank_mask[square::RANK_5]
          | bit::g_rank_mask[square::RANK_6]
          | bit::g_rank_mask[square::RANK_7]
          | bit::g_rank_mask[square::RANK_8];
        bit::Bitboard pawn = bd.piece(piece::PAWN, sd) + mask;
        const uint32 index = (uint32) (util::PEXT64(pawn.p(0),
              bit::g_rank_mask[square::RANK_9].p(0))
            + (util::PEXT64(pawn.p(1),
                bit::g_rank_mask[square::RANK_9].p(1)) << 7));
        //std::cout<<"index:"<<index<<std::endl;
        //std::cout<<bit::g_double_pawn_mask[sd][index]<<std::endl;
        bit::Bitboard pawn_target = target & bit::g_double_pawn_mask[sd][index];
        const auto mate_with_sq =
          (sd == side::BLACK) ?
          bd.king(side::opposit(sd)) + square::INC_DOWN :
          bd.king(side::opposit(sd)) + square::INC_UP;
        if (pawn_target.is_set(mate_with_sq)
            && attack::is_mate_with_pawn_drop(mate_with_sq, bd)) {
          pawn_target.clear(mate_with_sq);
        }
        while (!pawn_target.is_empty()) {
          ml.add(move::make(pawn_target.lsb(), piece::PAWN));
        }

      }

      //rank1
      bit::Bitboard rank_1 = target
        & bit::g_rank_mask[square::side_rank<sd>(square::RANK_1)];
      while (!rank_1.is_empty()) {
        const auto to = rank_1.lsb();
        for (auto i = 0; i < num; i++) {
          ml.add(move::make(to, p[i]));
        }
      }

      //rank2
      bit::Bitboard rank_2 = target
        & bit::g_rank_mask[square::side_rank<sd>(square::RANK_2)];
      while (!rank_2.is_empty()) {
        const auto to = rank_2.lsb();
        for (auto i = 0; i < num; i++) {
          ml.add(move::make(to, p[i]));
        }
        if (has_lance) {
          ml.add(move::make(to, piece::LANCE));
        }
      }

      //rank3~rank9
      bit::Bitboard rank_39 = target
        & ~(bit::g_rank_mask[square::side_rank<sd>(square::RANK_1)]
            | bit::g_rank_mask[square::side_rank<sd>(square::RANK_2)]);
      while (!rank_39.is_empty()) {
        const auto to = rank_39.lsb();
        for (auto i = 0; i < num; i++) {
          ml.add(move::make(to, p[i]));
        }
        if (has_lance) {
          ml.add(move::make(to, piece::LANCE));
        }
        if (has_knight) {
          ml.add(move::make(to, piece::KNIGHT));
        }
      }
    }
  template<int sd>
    void add_drop_move(List & ml, const board::Board & bd,
        const bit::Bitboard & target) {

      assert(bd.is_ok());

      const auto hand = bd.hand(sd);
      auto status = 0;
      int piece_list[4] = { };
      int * p = piece_list;

      (hand::has(hand, piece::PAWN)) ? status += 1 : status;
      (hand::has(hand, piece::LANCE)) ? status += 2 : status;
      (hand::has(hand, piece::KNIGHT)) ? status += 4 : status;
      (hand::has(hand, piece::SILVER)) ?
        status += 8, (*p++) = piece::SILVER : status;
      (hand::has(hand, piece::GOLD)) ? status += 8, (*p++) = piece::GOLD : status;
      (hand::has(hand, piece::BISHOP)) ?
        status += 8, (*p++) = piece::BISHOP : status;
      (hand::has(hand, piece::ROOK)) ? status += 8, (*p++) = piece::ROOK : status;

#define ADD_DROP_MOVE(n) {\
  constexpr auto knight_flag = (n & (1 << 2)) != 0;\
  constexpr auto lance_flag  = (n & (1 << 1)) != 0;\
  constexpr auto pawn_flag   = (n & (1 << 0)) != 0;\
  constexpr auto num = n >> 3;\
  add_drop_move<knight_flag,lance_flag,pawn_flag,sd,num>(ml,bd,target,piece_list); \
  break;\
}
      switch (status) {
        case 0:
          break;
        case 1:
          ADD_DROP_MOVE(1)
        case 2:
            ADD_DROP_MOVE(2)
        case 3:
              ADD_DROP_MOVE(3)
        case 4:
                ADD_DROP_MOVE(4)
        case 5:
                  ADD_DROP_MOVE(5)
        case 6:
                    ADD_DROP_MOVE(6)
        case 7:
                      ADD_DROP_MOVE(7)
        case 8:
                        ADD_DROP_MOVE(8)
        case 9:
                          ADD_DROP_MOVE(9)
        case 10:
                            ADD_DROP_MOVE(10)
        case 11:
                              ADD_DROP_MOVE(11)
        case 12:
                                ADD_DROP_MOVE(12)
        case 13:
                                  ADD_DROP_MOVE(13)
        case 14:
                                    ADD_DROP_MOVE(14)
        case 15:
                                      ADD_DROP_MOVE(15)
        case 16:
                                        ADD_DROP_MOVE(16)
        case 17:
                                          ADD_DROP_MOVE(17)
        case 18:
                                            ADD_DROP_MOVE(18)
        case 19:
                                              ADD_DROP_MOVE(19)
        case 20:
                                                ADD_DROP_MOVE(20)
        case 21:
                                                  ADD_DROP_MOVE(21)
        case 22:
                                                    ADD_DROP_MOVE(22)
        case 23:
                                                      ADD_DROP_MOVE(23)
        case 24:
                                                        ADD_DROP_MOVE(24)
        case 25:
                                                          ADD_DROP_MOVE(25)
        case 26:
                                                            ADD_DROP_MOVE(26)
        case 27:
                                                              ADD_DROP_MOVE(27)
        case 28:
                                                                ADD_DROP_MOVE(28)
        case 29:
                                                                  ADD_DROP_MOVE(29)
        case 30:
                                                                    ADD_DROP_MOVE(30)
        case 31:
                                                                      ADD_DROP_MOVE(31)
        case 32:
                                                                        ADD_DROP_MOVE(32)
        case 33:
                                                                          ADD_DROP_MOVE(33)
        case 34:
                                                                            ADD_DROP_MOVE(34)
        case 35:
                                                                              ADD_DROP_MOVE(35)
        case 36:
                                                                                ADD_DROP_MOVE(36)
        case 37:
                                                                                  ADD_DROP_MOVE(37)
        case 38:
                                                                                    ADD_DROP_MOVE(38)
        case 39:
                                                                                      ADD_DROP_MOVE(39)
        case 40:
                                                                                        ADD_DROP_MOVE(40)
        default:
                                                                                          assert(false);
      }
#undef ADD_DROP_MOVE
}
template<MoveType mt, int sd>
void add_move(List & ml, const board::Board & bd,
    const attack::Attacks * attacks = nullptr) {

  assert(bd.is_ok());
  assert(attack::is_legal(bd));

  constexpr int xd = side::opposit(sd);
  bit::Bitboard target, target2;
  switch (mt) {
    case TACTICAL:
      target = bd.side(xd);
      target2 = target | (~bd.side(sd) & bit::g_prom[sd]);
      break;
    case QUIET:
      target = ~bd.all();
      target2 = target & bit::g_middle[sd];
      break;
    case DROP:
      target = bd.all() ^ bit::g_all_one;
      break;
    case EVASION:
      assert(attacks != nullptr);
      assert(attacks->size == 1 || attacks->size == 2);
      target = ~bd.side(sd) & ~attacks->avoid;
      add_king_move<mt, sd>(ml, bd, target);
      if (attacks->size == 2) {
        return;
      }
      target = target2 = attack::g_between[attacks->square[0]][bd.king(sd)];
      target |= attacks->square[0];

      add_pawn_move<mt, sd>(ml, bd, target);
      add_lance_knight_move<piece::LANCE, mt, sd>(ml, bd, target);
      add_lance_knight_move<piece::KNIGHT, mt, sd>(ml, bd, target);
      add_silver_move<mt, sd>(ml, bd, target);
      add_noprom_move<piece::GOLDS, mt, sd>(ml, bd, target);
      add_bishop_rook_move<piece::BISHOP, mt, sd>(ml, bd, target);
      add_bishop_rook_move<piece::ROOK, mt, sd>(ml, bd, target);
      add_noprom_move<piece::PBISHOP, mt, sd>(ml, bd, target);
      add_noprom_move<piece::PROOK, mt, sd>(ml, bd, target);

      add_drop_move<sd>(ml, bd, target2);
      return;
    default:
      assert(false);
      break;
  }
  switch (mt) {
    case DROP:
      add_drop_move<sd>(ml, bd, target);
      break;
    default:
      add_pawn_move<mt, sd>(ml, bd, target2);
      add_lance_knight_move<piece::LANCE, mt, sd>(ml, bd, target);
      add_lance_knight_move<piece::KNIGHT, mt, sd>(ml, bd, target);
      add_silver_move<mt, sd>(ml, bd, target);
      add_noprom_move<piece::GOLDS, mt, sd>(ml, bd, target);
      add_king_move<mt, sd>(ml, bd, target);
      add_bishop_rook_move<piece::BISHOP, mt, sd>(ml, bd, target);
      add_bishop_rook_move<piece::ROOK, mt, sd>(ml, bd, target);
      add_noprom_move<piece::PBISHOP, mt, sd>(ml, bd, target);
      add_noprom_move<piece::PROOK, mt, sd>(ml, bd, target);
      break;
  }
}

template<int sd> void add_legal_move(List & ml, const board::Board & bd) {

  List ml2;
  attack::Attacks att;
  attack::init_attacks(att, bd);
  if (!attack::is_in_check(bd)) {
    add_move<TACTICAL, sd>(ml2, bd);
    add_move<QUIET, sd>(ml2, bd);
    add_move<DROP, sd>(ml2, bd);
  } else {
    add_move<EVASION, sd>(ml2, bd, &att);
  }
  for (auto pos = 0; pos < ml2.size(); ++pos) {
    const auto mv = ml2.move(pos);
    if (move::is_legal(mv, bd, att)) {
      ml.add(mv);
    }
  }
}
inline void add_legal_move(List & ml, const board::Board & bd) {
  return (bd.turn() == side::BLACK) ?
    add_legal_move<side::BLACK>(ml, bd) :
    add_legal_move<side::WHITE>(ml, bd);
}

inline int legal_move_num(const board::Board & bd) {
  List ml;
  add_legal_move(ml,bd);
  return ml.size();
}

bool is_move(const int mv, const board::Board & bd) { // for TT-move legality
  const auto sd = bd.turn();
  const auto xd = side::opposit(sd);
  const auto from = move::from(mv);
  const auto to = move::to(mv);
  const auto pc = move::piece(mv);
  const auto cp = move::cap(mv);
  const auto prom = move::prom(mv);
  if (move::is_drop(mv)) {
    if (!hand::has(bd.hand(sd), pc)) {
      return false;
    }
    if (bd.square(to) != piece::NONE) {
      return false;
    }
    if (pc == piece::KNIGHT) {
      const auto rank = square::rank(to);
      if (sd == side::BLACK && rank <= square::RANK_2) {
        return false;
      } else if (sd == side::WHITE && rank >= square::RANK_8) {
        return false;
      }
    } else if (pc == piece::LANCE) {
      const auto rank = square::rank(to);
      if (sd == side::BLACK && rank == square::RANK_1) {
        return false;
      } else if (sd == side::WHITE && rank == square::RANK_9) {
        return false;
      }
    } else if (pc == piece::PAWN) {
      const auto file = square::file(to);
      const auto rank = square::rank(to);
      if (sd == side::BLACK && rank == square::RANK_1) {
        return false;
      } else if (sd == side::WHITE && rank == square::RANK_9) {
        return false;
      }
      if (!(bd.piece(piece::PAWN, sd) & bit::g_file_mask[file]).is_empty()) {
        return false;
      }
      const auto inc =
        (sd == side::BLACK) ? square::INC_DOWN : square::INC_UP;
      if ((bd.king(xd) + inc) == to
          && attack::is_mate_with_pawn_drop(to, bd)) {
        return false;
      }
    }
  } else {
    if(pc == piece::NONE) {
      return false;
    }
    if (bd.square(from) != pc) {
      return false;
    }
    if (bd.square_side(from) != sd) {
      return false;
    }
    if (bd.square(to) != piece::NONE && bd.square_side(to) == sd) {
      return false;
    }
    if (bd.square(to) != cp) {
      return false;
    }
    if (prom) {
      if (pc == piece::ROOK || pc == piece::BISHOP
          || pc == piece::SILVER) {
        if (!square::is_promotion(sd, from)
            && !square::is_promotion(sd, to)) {
          return false;
        }
      } else if (pc == piece::PAWN || pc == piece::LANCE
          || pc == piece::KNIGHT) {
        if (!square::is_promotion(sd, to)) {
          return false;
        }
      } else {
        return false;
      }
    }
    bit::Bitboard att = attack::pseudo_attack_from(pc,sd,from);
    if(!att.is_set(to)) {
      return false;
    }
    if(!(attack::g_between[from][to] & bd.all()).is_empty()) {
      return false;
    }
  }
  return true;
}

bool is_quiet_move(const int mv, const board::Board & bd) { // for killerlegality
  if(!is_move(mv,bd)) {
    return false;
  }
  return !move::is_tactical(mv);
}

}
namespace move {
  bool is_ok(const int mv, const board::Board & bd) {

    const auto sd = bd.turn();
    const auto from = move::from(mv);
    const auto to = move::to(mv);
    const auto pc = move::piece(mv);
    const auto cp = move::cap(mv);
    const auto prom = move::prom(mv);

    if (mv == move::NONE) {
      return false;
    }
    if (mv == move::NULL_) {
      return false;
    }
    if (!move::is_drop(mv)) {
      bit::Bitboard att = attack::pseudo_attack_from(pc, sd, from);
      if (!att.is_set(to)) {
        return false;
      }
      if (!(attack::g_between[from][to] & bd.all()).is_empty()) {
        return false;
      }
      if (prom) {
        if (!piece::can_prom(pc)) {
          return false;
        }
      }
    } else {
      if (cp != piece::NONE) {
        return false;
      }
      if (prom) {
        return false;
      }
    }

    return gen::is_move(mv, bd);
  }
}
namespace pst {
  void init() {
  }
}
namespace pawn {
  void init() {
  }
}
namespace material {
  int score(const int p) {
    return piece::value(p);
  }
  void init() {

  }
}
namespace eval {

  static const std::string KKP_NAME = "./kkp.bin";
  static const std::string KPP_NAME = "./kpp.bin";

  enum {
    f_hand_pawn   =    0,
    e_hand_pawn   =   f_hand_pawn + 18 + 1,
    f_hand_lance  =   e_hand_pawn + 18 + 1,
    e_hand_lance  =   f_hand_lance + 4 + 1,
    f_hand_knight =   e_hand_lance + 4 + 1,
    e_hand_knight =   f_hand_knight + 4 + 1,
    f_hand_silver =   e_hand_knight + 4 + 1,
    e_hand_silver =   f_hand_silver + 4 + 1,
    f_hand_gold   =   e_hand_silver + 4 + 1,
    e_hand_gold   =   f_hand_gold + 4 + 1,
    f_hand_bishop =   e_hand_gold + 4 + 1,
    e_hand_bishop =   f_hand_bishop + 4 + 1,
    f_hand_rook   =   e_hand_bishop + 4 + 1,
    e_hand_rook   =   f_hand_rook + 2 + 1,
    fe_hand_end   =   e_hand_rook + 2 + 1,
    f_pawn        =   fe_hand_end,
    e_pawn        =  f_pawn + 81,
    f_lance       =  e_pawn + 81,
    e_lance       =  f_lance + 81,
    f_knight      =  e_lance + 81,
    e_knight      =  f_knight + 81,
    f_silver      =  e_knight + 81,
    e_silver      =  f_silver + 81,
    f_gold        =  e_silver + 81,
    e_gold        =  f_gold + 81,
    f_bishop      =  e_gold + 81,
    e_bishop      =  f_bishop + 81,
    f_horse       =  e_bishop + 81,
    e_horse       =  f_horse + 81,
    f_rook        =  e_horse + 81,
    e_rook        =  f_rook + 81,
    f_dragon      =  e_rook + 81,
    e_dragon      =  f_dragon + 81,
    fe_end        =  e_dragon + 81,

    kkp_hand_pawn   =   0,
    kkp_hand_lance  =  kkp_hand_pawn + 18 + 1,
    kkp_hand_knight =  kkp_hand_lance + 4 + 1,
    kkp_hand_silver =  kkp_hand_knight + 4 + 1,
    kkp_hand_gold   =  kkp_hand_silver + 4 + 1,
    kkp_hand_bishop =  kkp_hand_gold + 4 + 1,
    kkp_hand_rook   =  kkp_hand_bishop + 2 + 1,
    kkp_hand_end   =   kkp_hand_rook + 2 + 1,
    kkp_pawn        =  kkp_hand_end,
    kkp_lance       =  kkp_pawn + 81,
    kkp_knight      =  kkp_lance + 81,
    kkp_silver      =  kkp_knight + 81,
    kkp_gold        =  kkp_silver + 81,
    kkp_bishop      =  kkp_gold + 81,
    kkp_horse       =  kkp_bishop + 81,
    kkp_rook        =  kkp_horse + 81,
    kkp_dragon      =  kkp_rook + 81,
    kkp_end         =  kkp_dragon + 81 };

  short pc_on_sq[square::SIZE][fe_end][fe_end];
  short kkp[square::SIZE][square::SIZE][kkp_end];

  int comp_eval (const board::Board & bd);

  struct Entry {
    uint32 lock;
    int eval;
  };

  class Table {

    private:

      static const int BITS = 16;
      static const int SIZE = 1 << BITS;
      static const int MASK = SIZE - 1;

      Entry p_table[SIZE];

    public:

      void clear() {
        for (int index = 0; index < SIZE; index++) {
          p_table[index].lock = 0;
          p_table[index].eval = 0;
        }
      }

      int eval(const board::Board & bd) { // NOTE: score for black

        assert(bd.is_ok());

        const hash_t key = bd.key() ^ bd.hand(side::BLACK);

        int    index = hash::index(key) & MASK;
        uint32 lock  = hash::lock(key);

        Entry & entry = p_table[index];

        if (entry.lock == lock) {
          return entry.eval / score::FV_SCALE;
        }

        int eval = comp_eval(bd);

        entry.lock = lock;
        entry.eval = eval;

        return eval / score::FV_SCALE;
      }

  };

  uint32 make_list(const board::Board &bd, int list0[], int list1[], short &score) {

    uint32 nlist = 14;
    const auto bking = bd.king(side::BLACK);
    const auto wking = bd.king(side::WHITE);
    const auto bking2 = square::opposit_sq(bking);
    const auto wking2 = square::opposit_sq(wking);

    auto hand_b = bd.hand(side::BLACK);
    auto hand_w = bd.hand(side::WHITE);

    list0[ 0] = f_hand_pawn   + hand::num(hand_b,piece::PAWN);
    list0[ 1] = e_hand_pawn   + hand::num(hand_w,piece::PAWN);
    list0[ 2] = f_hand_lance  + hand::num(hand_b,piece::LANCE);
    list0[ 3] = e_hand_lance  + hand::num(hand_w,piece::LANCE);
    list0[ 4] = f_hand_knight + hand::num(hand_b,piece::KNIGHT);
    list0[ 5] = e_hand_knight + hand::num(hand_w,piece::KNIGHT);
    list0[ 6] = f_hand_silver + hand::num(hand_b,piece::SILVER);
    list0[ 7] = e_hand_silver + hand::num(hand_w,piece::SILVER);
    list0[ 8] = f_hand_gold   + hand::num(hand_b,piece::GOLD);
    list0[ 9] = e_hand_gold   + hand::num(hand_w,piece::GOLD);
    list0[10] = f_hand_bishop + hand::num(hand_b,piece::BISHOP);
    list0[11] = e_hand_bishop + hand::num(hand_w,piece::BISHOP);
    list0[12] = f_hand_rook   + hand::num(hand_b,piece::ROOK);
    list0[13] = e_hand_rook   + hand::num(hand_w,piece::ROOK);

    list1[ 0] = f_hand_pawn   + hand::num(hand_w,piece::PAWN);
    list1[ 1] = e_hand_pawn   + hand::num(hand_b,piece::PAWN);
    list1[ 2] = f_hand_lance  + hand::num(hand_w,piece::LANCE);
    list1[ 3] = e_hand_lance  + hand::num(hand_b,piece::LANCE);
    list1[ 4] = f_hand_knight + hand::num(hand_w,piece::KNIGHT);
    list1[ 5] = e_hand_knight + hand::num(hand_b,piece::KNIGHT);
    list1[ 6] = f_hand_silver + hand::num(hand_w,piece::SILVER);
    list1[ 7] = e_hand_silver + hand::num(hand_b,piece::SILVER);
    list1[ 8] = f_hand_gold   + hand::num(hand_w,piece::GOLD);
    list1[ 9] = e_hand_gold   + hand::num(hand_b,piece::GOLD);
    list1[10] = f_hand_bishop + hand::num(hand_w,piece::BISHOP);
    list1[11] = e_hand_bishop + hand::num(hand_b,piece::BISHOP);
    list1[12] = f_hand_rook   + hand::num(hand_w,piece::ROOK);
    list1[13] = e_hand_rook   + hand::num(hand_b,piece::ROOK);

    score += kkp[bking] [wking] [kkp_hand_pawn   + hand::num(hand_b,piece::PAWN)];
    score += kkp[bking] [wking] [kkp_hand_lance  + hand::num(hand_b,piece::LANCE)];
    score += kkp[bking] [wking] [kkp_hand_knight + hand::num(hand_b,piece::KNIGHT)];
    score += kkp[bking] [wking] [kkp_hand_silver + hand::num(hand_b,piece::SILVER)];
    score += kkp[bking] [wking] [kkp_hand_gold   + hand::num(hand_b,piece::GOLD)];
    score += kkp[bking] [wking] [kkp_hand_bishop + hand::num(hand_b,piece::BISHOP)];
    score += kkp[bking] [wking] [kkp_hand_rook   + hand::num(hand_b,piece::ROOK)];

    score -= kkp[wking2] [bking2] [kkp_hand_pawn   + hand::num(hand_w,piece::PAWN)];
    score -= kkp[wking2] [bking2] [kkp_hand_lance  + hand::num(hand_w,piece::LANCE)];
    score -= kkp[wking2] [bking2] [kkp_hand_knight + hand::num(hand_w,piece::KNIGHT)];
    score -= kkp[wking2] [bking2] [kkp_hand_silver + hand::num(hand_w,piece::SILVER)];
    score -= kkp[wking2] [bking2] [kkp_hand_gold   + hand::num(hand_w,piece::GOLD)];
    score -= kkp[wking2] [bking2] [kkp_hand_bishop + hand::num(hand_w,piece::BISHOP)];
    score -= kkp[wking2] [bking2] [kkp_hand_rook   + hand::num(hand_w,piece::ROOK)];

    //pawn
    bit::Bitboard piece = bd.piece(piece::PAWN,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_pawn + sq;
      list1[nlist] = e_pawn + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_pawn + sq];
      ++nlist;
    }
    piece = bd.piece(piece::PAWN,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_pawn + sq;
      list1[nlist] = f_pawn + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_pawn + square::opposit_sq(sq)];
      ++nlist;
    }

    //lance
    piece = bd.piece(piece::LANCE,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_lance + sq;
      list1[nlist] = e_lance + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_lance + sq];
      ++nlist;
    }
    piece = bd.piece(piece::LANCE,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_lance + sq;
      list1[nlist] = f_lance + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_lance + square::opposit_sq(sq)];
      ++nlist;
    }

    //knight
    piece = bd.piece(piece::KNIGHT,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_knight + sq;
      list1[nlist] = e_knight + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_knight + sq];
      ++nlist;
    }
    piece = bd.piece(piece::KNIGHT,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_knight + sq;
      list1[nlist] = f_knight + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_knight + square::opposit_sq(sq)];
      ++nlist;
    }


    //silver
    piece = bd.piece(piece::SILVER,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_silver + sq;
      list1[nlist] = e_silver + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_silver + sq];
      ++nlist;
    }
    piece = bd.piece(piece::SILVER,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_silver + sq;
      list1[nlist] = f_silver + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_silver + square::opposit_sq(sq)];
      ++nlist;
    }
    //gold
    piece = bd.golds(side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_gold + sq;
      list1[nlist] = e_gold + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_gold + sq];
      ++nlist;
    }
    piece = bd.golds(side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_gold + sq;
      list1[nlist] = f_gold + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_gold + square::opposit_sq(sq)];
      ++nlist;
    }

    //bishop

    piece = bd.piece(piece::BISHOP,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_bishop + sq;
      list1[nlist] = e_bishop + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_bishop + sq];
      ++nlist;
    }
    piece = bd.piece(piece::BISHOP,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_bishop + sq;
      list1[nlist] = f_bishop + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_bishop + square::opposit_sq(sq)];
      ++nlist;
    }


    //horse

    piece = bd.piece(piece::PBISHOP,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_horse + sq;
      list1[nlist] = e_horse + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_horse + sq];
      ++nlist;
    }
    piece = bd.piece(piece::PBISHOP,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_horse + sq;
      list1[nlist] = f_horse + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_horse + square::opposit_sq(sq)];
      ++nlist;
    }

    //rook

    piece = bd.piece(piece::ROOK,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_rook + sq;
      list1[nlist] = e_rook + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_rook + sq];
      ++nlist;
    }
    piece = bd.piece(piece::ROOK,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_rook + sq;
      list1[nlist] = f_rook + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_rook + square::opposit_sq(sq)];
      ++nlist;
    }

    //dragon

    piece = bd.piece(piece::PROOK,side::BLACK);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = f_dragon + sq;
      list1[nlist] = e_dragon + square::opposit_sq(sq);
      score += kkp[bking][wking][kkp_dragon + sq];
      ++nlist;
    }
    piece = bd.piece(piece::PROOK,side::WHITE);
    while(!piece.is_empty()) {
      const auto sq = piece.lsb();
      list0[nlist] = e_dragon + sq;
      list1[nlist] = f_dragon + square::opposit_sq(sq);
      score -= kkp[wking2][bking2][kkp_dragon + square::opposit_sq(sq)];
      ++nlist;
    }

    return nlist;
  }

  int comp_eval(const board::Board & bd) { // NOTE: score for black

    int eval_material = 0;

    for(bit::Bitboard bb = bd.side(side::BLACK); !bb.is_empty();) {
      const auto sq = bb.lsb();
      eval_material += material::score(bd.square(sq));
    }

    const auto hand_b = bd.hand(side::BLACK);

    eval_material += material::score(piece::PAWN) * hand::num(hand_b,piece::PAWN);
    eval_material += material::score(piece::LANCE) * hand::num(hand_b,piece::LANCE);
    eval_material += material::score(piece::KNIGHT) * hand::num(hand_b,piece::KNIGHT);
    eval_material += material::score(piece::SILVER) * hand::num(hand_b,piece::SILVER);
    eval_material += material::score(piece::GOLD) * hand::num(hand_b,piece::GOLD);
    eval_material += material::score(piece::BISHOP) * hand::num(hand_b,piece::BISHOP);
    eval_material += material::score(piece::ROOK) * hand::num(hand_b,piece::ROOK);

    for(bit::Bitboard bb = bd.side(side::WHITE); !bb.is_empty();) {
      const auto sq = bb.lsb();
      eval_material -= material::score(bd.square(sq));
    }

    const auto hand_w = bd.hand(side::WHITE);

    eval_material -= material::score(piece::PAWN) * hand::num(hand_w,piece::PAWN);
    eval_material -= material::score(piece::LANCE) * hand::num(hand_w,piece::LANCE);
    eval_material -= material::score(piece::KNIGHT) * hand::num(hand_w,piece::KNIGHT);
    eval_material -= material::score(piece::SILVER) * hand::num(hand_w,piece::SILVER);
    eval_material -= material::score(piece::GOLD) * hand::num(hand_w,piece::GOLD);
    eval_material -= material::score(piece::BISHOP) * hand::num(hand_w,piece::BISHOP);
    eval_material -= material::score(piece::ROOK) * hand::num(hand_w,piece::ROOK);


    eval_material *= score::FV_SCALE;

    int list0[52],list1[52];
    short kkp_score = 0;
    uint32 nlist = make_list(bd,list0,list1,kkp_score);
    const auto bking = bd.king(side::BLACK);
    const auto wking = square::opposit_sq(bd.king(side::WHITE));

    auto kpp_score = 0;
    for(uint32 i = 0; i < nlist; ++i){
      const auto k0 = list0[i];
      const auto k1 = list1[i];
      for(auto j = 0; j <= i; ++j){
        const auto l0 = list0[j];
        const auto l1 = list1[j];
        kpp_score += pc_on_sq[bking][k0][l0];
        kpp_score -= pc_on_sq[wking][k1][l1];
      }
    }

    auto all_score = (eval_material + kkp_score + kpp_score);
    all_score += util::rand_int(50);
    assert(all_score / score::FV_SCALE >= score::EVAL_MIN && all_score / score::FV_SCALE <= score::EVAL_MAX);
    return all_score;
  }

  int eval(board::Board & bd, Table & table) {
    return score::side_score(table.eval(bd), bd.turn());
  }
  void clear() {
    std::memset(pc_on_sq, 0, sizeof(pc_on_sq));
    std::memset(kkp, 0, sizeof(kkp));
  }
  void init() {

    util::Tee<<"load fv.bin...";
    clear();
    std::ifstream fin1(KPP_NAME, std::ios::in | std::ios::binary);
    std::ifstream fin2(KKP_NAME, std::ios::in | std::ios::binary);
    if(!fin1) {
      util::Tee<<"open kpp.bin error\n";
      exit(1);
    }
    if(!fin2) {
      util::Tee<<"open kkp.bin error\n";
      exit(1);
    }

    fin1.read((char*)pc_on_sq,sizeof(pc_on_sq));
    fin2.read((char*)kkp, sizeof(kkp));


    fin1.close();
    fin2.close();

    util::Tee<<"end\n";
  }
}
namespace trans {

  struct Entry { // 16 bytes
    uint32 lock;
    uint32 hand_b;
    uint16 move;
    int16 score;
    int16 depth;
    uint8 date;
    uint8 flags;
  };

  static_assert(sizeof(Entry) == 16,"trans::Entry size error");

  void clear_entry(Entry & entry) {

    assert(sizeof(Entry) == 16);

    entry.lock = 0;
    entry.hand_b = 0;
    entry.move = move::NONE;
    entry.score = 0;
    entry.date = 0;
    entry.depth = -1;
    entry.flags = score::FLAGS_NONE;

  }

  class Table {

    private:

      Entry * p_table;
      int p_bits;
      uint64 p_size;
      uint64 p_mask;

      int p_date;
      uint64 p_used;

      int size_to_bits(int size) {

        int bits = 0;

        for (uint64 entries = (uint64(size) << 20) / sizeof(Entry); entries > 1; entries /= 2) {
          bits++;
        }

        return bits;
      }

    public:

      Table() {

        p_table = NULL;
        p_bits = 0;
        p_size = 1;
        p_mask = 0;

        p_date = 0;
        p_used = 0;
      }

      void set_size(int size) {

        int bits = size_to_bits(size);
        if (bits == p_bits) return;

        p_bits = bits;
        p_size = U64(1) << bits;
        p_mask = p_size - 1;

        if (p_table != NULL) {
          free();
          alloc();
        }
      }

      void alloc() {
        assert(p_table == NULL);
        p_table = new Entry[p_size];
        clear();
      }

      void free() {
        assert(p_table != NULL);
        delete [] p_table;
        p_table = NULL;
      }

      void clear() {

        Entry e;
        clear_entry(e);

        for (uint64 i = 0; i < p_size; i++) {
          p_table[i] = e;
        }

        for (uint64 i = 0; i < p_size; i++) {
          assert(p_table[i].date == 0);
        }


        p_date = 1;
        p_used = 0;
      }

      void inc_date() {
        p_date = (p_date + 1) % 256;
        p_used = 0;
      }

      void store(const board::Board & bd, int depth, int ply, int move, int score, int flags) {

        assert(depth >= 0 && depth < 100);
        assert(move != move::NULL_);
        assert(score >= score::MIN && score <= score::MAX);

        score = score::to_trans(score, ply);

        hash_t key = bd.key();
        uint32 hand_b = bd.hand(side::BLACK);

        uint64 index = hash::index(key) & p_mask;
        uint32 lock  = hash::lock(key);

        Entry * be = NULL;
        int bs = -1;

        for (uint64 i = 0; i < 4; i++) {

          uint64 idx = (index + i) & p_mask;
          assert(idx < p_size);
          Entry & entry = p_table[idx];

          if ((entry.lock == lock) && (entry.hand_b == hand_b)) {

            if (entry.date != p_date) {
              entry.date = p_date;
              p_used++;
            }

            if (depth >= entry.depth) {
              if (move != move::NONE){
                entry.move = move::move_to_move16(move);
              }
              entry.depth = depth;
              entry.score = score;
              entry.flags = flags;
            } else if (entry.move == move::NONE) {
              entry.move = move::move_to_move16(move);
            }
#ifdef TRACE
            util::Tee<<"update trans:"<<":idx:"<<idx<<":"<<bd.key()<<":"<<hand_b<<":"<<move::to_can2(move)<<":"<<score<<":"<<flags<<std::endl;
            util::Tee<<"update trans:"<<":idx:"<<idx<<":"<<entry.lock<<":"<<entry.hand_b<<":"<<move::to_can2(entry.move)<<":"<<entry.score<<":"<<entry.flags<<std::endl;
#endif

            return;
          }

          int sc = 99 - entry.depth; // NOTE: entry.depth can be -1
          if (entry.date != p_date) sc += 101;
          assert(sc >= 0 && sc < 202);

          if (sc > bs) {
            be = &entry;
            bs = sc;
          }
        }

        assert(be != NULL);

        if (be->date != p_date) p_used++;

        be->lock = lock;
        be->hand_b = hand_b;
        be->date = p_date;
        be->move = move::move_to_move16(move);
        be->depth = depth;
        be->score = score;
        be->flags = flags;
#ifdef TRACE
        util::Tee<<"store trans:"<< ":"<<key<<":"<<lock<<":"<<hand_b<<":"<<move::to_can2(move)<<":"<<score<<":"<<flags<<std::endl;
#endif
        assert(be->score == score);

      }

      bool retrieve(const board::Board & bd, int depth, int ply, int & move, int & score, int & flags) {

        assert(depth >= 0 && depth < 100);

        hash_t key = bd.key();
        uint32 hand_b = bd.hand(side::BLACK);

        uint64 index = hash::index(key) & p_mask;
        uint32 lock  = hash::lock(key);

        for (uint64 i = 0; i < 4; i++) {

          uint64 idx = (index + i) & p_mask;
          assert(idx < p_size);
          Entry & entry = p_table[idx];

          if ((entry.lock == lock) && (entry.hand_b == hand_b)) {

#ifdef TRACE
            util::Tee<<"same:"<<key<<":"<<lock<<":"<<hand_b<<":"<<i<<": idx:"<<idx<<std::endl;;
#endif

            if (entry.date != p_date) {
              entry.date = p_date; // touch entry
              p_used++;
            }

            move = move::move16_to_move(entry.move,bd);
            score = score::from_trans(entry.score, ply);
            flags = entry.flags;

            if (entry.depth >= depth) {
#ifdef TRACE
              util::Tee<<"hit trans: idx:"<<idx<<":"<<key<<":"<<lock<<":"<<hand_b<<":"<<move::to_can2(move)<<":"<<score<<":"<<flags<<std::endl;;
#endif
              return true;

            } else if (score::is_mate(score)) {
#ifdef TRACE
              util::Tee<<"hit trans2:"<<key<<":"<<move::to_can2(move)<<":"<<score<<":"<<flags<<std::endl;;
#endif
              flags &= ~(score < 0 ? score::FLAGS_LOWER : score::FLAGS_UPPER);

              return true;
            }

            return false;
          }
        }

        return false;
      }

      int used() const {
        return int((p_used * 1000 + p_size / 2) / p_size);
      }

  };

}

namespace sort {
  class Killer {
    private:

      static const int PLY = 100;

      struct List {
        int k0;
        int k1;
      };

      List killer_[PLY];

    public:

      void clear() {
        for (int ply = 0; ply < PLY; ++ply) {
          killer_[ply].k0 = move::NONE;
          killer_[ply].k1 = move::NONE;
        }
      }

      void add(const int mv, const int ply) {
        if (killer_[ply].k0 != mv) {
          killer_[ply].k1 = killer_[ply].k0;
          killer_[ply].k0 = mv;
        }
      }

      int killer_0(const int ply) const {
        return killer_[ply].k0;
      }

      int killer_1(const int ply) const {
        return killer_[ply].k1;
      }
  };
  class History {
    private:

      static const int PROB_BIT   = 11;
      static const int PROB_ONE   = 1 << PROB_BIT;
      static const int PROB_HALF  = 1 << (PROB_BIT - 1);
      static const int PROB_SHIFT = 5;

      std::array<std::array<std::array<int,piece::SIZE>,square::SIZE>,side::SIZE> prob_;

      void update_good(int mv, const board::Board & bd) {
        if (!move::is_tactical(mv)) {
          const auto sd = bd.turn();
          const auto to = move::to(mv);
          const auto pc = move::piece(mv);
          prob_[sd][to][pc] += (PROB_ONE - prob_[sd][to][pc]) >> PROB_SHIFT;
        }
      }

      void update_bad(int mv, const board::Board & bd) {
        if (!move::is_tactical(mv)) {
          const auto sd = bd.turn();
          const auto to = move::to(mv);
          const auto pc = move::piece(mv);
          prob_[sd][to][pc] -= prob_[sd][to][pc] >> PROB_SHIFT;
        }
      }

    public:

      void clear() {
        for(auto & sd : prob_) {
          for(auto & sq : sd) {
            for(auto & pc : sq) {
              pc = PROB_HALF;
            }
          }
        }
      }

      void add(int bm, const gen::List & searched, const board::Board & bd) {

        assert(bm != move::NONE);

        update_good(bm, bd);

        for (auto pos = 0; pos < searched.size(); ++pos) {
          auto mv = searched.move(pos);
          if (mv != bm) update_bad(mv, bd);
        }
      }

      int score(int mv, const board::Board & bd) const {
        const auto sd = bd.turn();
        const auto to = move::to(mv);
        const auto pc = move::piece(mv);
        return prob_[sd][to][pc];
      }
  };
  int tactical_score(const int mv) {
    assert(move::is_tactical(mv));
    auto score = 1024;
    if(move::is_capture(mv)) {
      const auto cp = move::cap(mv);
      const auto pc = move::piece(mv);
      score += (piece::score(cp) + piece::SIZE) - piece::score(pc);
    }
    if(move::prom(mv)) {
      score++;
    }

    return score;
  }

  int evasion_score(int mv, int trans_move) {

    int sc;

    if (false) {
    } else if (mv == trans_move) {
      sc = move::SCORE_MASK;
    } else if (move::is_tactical(mv)) {
      sc = tactical_score(mv) + 1024;
      //assert (sc >= 1 && sc < 41);
    } else {
      sc = 0;
    }

    return sc;
  }
  void sort_tacticals(gen::List & ml) {

    for (auto pos = 0; pos < ml.size(); ++pos) {
      int mv = ml.move(pos);
      int sc = tactical_score(mv);
      ml.set_score(pos, sc);
    }

    ml.sort();
  }
  void sort_history(gen::List & ml, const board::Board & bd, const History & history) {

    for (auto pos = 0; pos < ml.size(); ++pos) {
      int mv = ml.move(pos);
      int sc = history.score(mv, bd);
      ml.set_score(pos, sc);
    }

    ml.sort();
  }

  void sort_evasions(gen::List & ml, int trans_move) {

    for (auto pos = 0; pos < ml.size(); pos++) {
      const auto mv = ml.move(pos);
      const auto sc = evasion_score(mv, trans_move);
      ml.set_score(pos, sc);
    }

    ml.sort();
  }
}
namespace gensort {

  // HACK: outside of List class because of C++ "static const" limitations :(

  enum Inst {
    GEN_EVASION,
    GEN_TRANS,
    GEN_TACTICAL,
    GEN_KILLER,
    GEN_CHECK,
    GEN_PAWN,
    GEN_QUIET,
    GEN_BAD,
    GEN_END,
    POST_MOVE,
    POST_MOVE_SEE,
    POST_KILLER,
    POST_KILLER_SEE,
    POST_BAD,
  };

  //const Inst g_prog_main[]    = { GEN_TRANS, POST_KILLER, GEN_TACTICAL, POST_MOVE_SEE, GEN_KILLER, POST_KILLER_SEE, GEN_QUIET, POST_MOVE_SEE, GEN_BAD, POST_BAD, GEN_END };
  const Inst g_prog_main[]    = { GEN_TRANS, POST_KILLER, GEN_TACTICAL, POST_MOVE_SEE, GEN_KILLER, POST_KILLER, GEN_QUIET, POST_MOVE, GEN_BAD, POST_BAD, GEN_END };
  //const Inst g_prog_QS_Root[] = { GEN_TRANS, POST_KILLER, GEN_TACTICAL, POST_MOVE, GEN_CHECK, POST_KILLER, GEN_PAWN, POST_MOVE, GEN_END };
  const Inst g_prog_QS[]      = { GEN_TRANS, POST_KILLER, GEN_TACTICAL, POST_MOVE, GEN_END };
  const Inst g_prog_evasion[] = { GEN_EVASION, POST_MOVE_SEE, GEN_BAD, POST_BAD, GEN_END };


  class List {
    private:

      board::Board * board_;
      const attack::Attacks * attacks_;
      const sort::Killer * killer_;
      const sort::History * hist_;
      int trans_move_;

      const Inst * ip_;
      Inst gen_;
      Inst post_;

      gen::List todo_;
      gen::List done_;
      gen::List bad_;

      int pos_;
      bool candidate_;

      template<int sd>bool gen() {

        todo_.clear();
        pos_ = 0;

        switch (gen_) { {

        } case GEN_EVASION: {

          gen::add_move<gen::EVASION,sd>(todo_, *board_, attacks_);
          sort::sort_evasions(todo_, trans_move_);
          break;

        } case GEN_TRANS: {

          int mv = trans_move_;

          if (mv != move::NONE && gen::is_move(mv, *board_)) {
            todo_.add(mv);
          }

          candidate_ = true;

          break;

        } case GEN_TACTICAL: {

          gen::add_move<gen::TACTICAL,sd>(todo_, *board_);
          sort::sort_tacticals(todo_);

          candidate_ = true;

          break;

        } case GEN_KILLER: {

          int k0 = killer_->killer_0(board_->ply());

          if (k0 != move::NONE && gen::is_quiet_move(k0, *board_)) {
            todo_.add(k0);
          }

          int k1 = killer_->killer_1(board_->ply());

          if (k1 != move::NONE && gen::is_quiet_move(k1, *board_)) {
            todo_.add(k1);
          }

          candidate_ = true;

          break;

        } case GEN_QUIET: {

          gen::add_move<gen::QUIET,sd>(todo_, *board_);
          gen::add_move<gen::DROP,sd>(todo_, *board_);
          sort::sort_history(todo_, *board_, *hist_);

          candidate_ = false;

          break;

        } case GEN_BAD: {

          todo_ = bad_;

          candidate_ = false;

          break;

        } case GEN_END: {

          return false;

        } default: {

          assert(false);

        } }

        return true;
      }

      bool post(int mv) {

        assert(mv != move::NONE);

        switch (post_) { {

        } case POST_MOVE: {

          if (done_.contain(mv)) {
            return false;
          }

          if (!move::is_legal(mv, *board_, *attacks_)) {
            return false;
          }

          break;

        } case POST_MOVE_SEE: {

          if (done_.contain(mv)) {
            return false;
          }

          if (!move::is_legal(mv, *board_, *attacks_)) {
            return false;
          }

          if (!move::is_safe(mv, *board_)) {
            bad_.add(mv);
            return false;
          }

          break;

        } case POST_KILLER: {

          if (done_.contain(mv)) {
            return false;
          }

          if (!move::is_legal(mv, *board_, *attacks_)) {
            return false;
          }

          done_.add(mv);

          break;

        } case POST_KILLER_SEE: {

          if (done_.contain(mv)) {
            return false;
          }

          if (!move::is_legal(mv, *board_, *attacks_)) {
            return false;
          }

          done_.add(mv);

          if (!move::is_safe(mv, *board_)) {
            bad_.add(mv);
            return false;
          }

          break;

        } case POST_BAD: {

          assert(move::is_legal(mv, *board_, *attacks_));

          break;

        } default: {

          assert(false);

        } }

        return true;
      }

    public:

      void init(int depth, board::Board & bd, const attack::Attacks & attacks, int trans_move, const sort::Killer & killer, const sort::History & history) {

        board_ = &bd;
        attacks_ = &attacks;
        killer_ = &killer;
        hist_ = &history;
        trans_move_ = trans_move;

        if (false) {
        } else if (attacks.size != 0) { // in check
          ip_ = g_prog_evasion;
        } else if (depth <= 0) {
          ip_ = g_prog_QS;
        }/* else if (depth == 0) {
            p_ip = g_prog_QS_Root;
            } else if (use_fp) {
            p_ip = g_prog_QS_Root;
            } */else {
              ip_ = g_prog_main;
            }

      todo_.clear();
      done_.clear();
      bad_.clear();

      pos_ = 0;
      candidate_ = false;
      }

      template<int sd>int next() {

        while (true) {

          while (pos_ >= todo_.size()) {

            gen_  = *ip_++;
            post_ = *ip_++;

            if (!gen<sd>()) return move::NONE;
          }

          int mv = todo_.move(pos_++);
          if (post(mv)) return mv;
        }
      }

      bool is_candidate() const {
        return candidate_;
      }
  };
}
namespace usi {
  bool g_infinite; // for USI-design mistake :(
}

namespace mate1 {

  template<int sd>bool is_ok(const int move, board::Board &bd){

    //詰まないと言って詰んでる
    if(!move){
      if(!bd.is_ok()) {
        std::cout<<"mate1ply board is ok error\n";
        std::cout<<bd<<std::endl;
        return false;
      }
      gen::List ml;
      attack::Attacks at;
      attack::init_attacks(at,bd);
      gen::add_move<gen::TACTICAL,sd>(ml,bd,&at);
      gen::add_move<gen::QUIET,sd>(ml,bd,&at);
      gen::add_move<gen::DROP,sd>(ml,bd,&at);
      auto is_nomate = true;
      const auto opp_king_sq = bd.king(side::opposit(sd));
      const auto opp_king_att = attack::get_king_attack(opp_king_sq);
      for(auto i = 0, size = ml.size(); i < size; i++) {
        //玉の周りの王手しかダメ
        if(move::is_legal(ml.move(i),bd,at)
            && move::is_direck_check(ml.move(i),bd)
            && opp_king_att.is_set(move::to(ml.move(i)))) {
          bd.move(ml.move(i),true);
          gen::List ml2;
          attack::Attacks at2;
          attack::init_attacks(at2,bd);
          if(at2.size == 0) {
            std::cout<<bd<<std::endl;
            std::cout<<move::to_can2(ml.move(i))<<std::endl;
            assert(false);
          }
          gen::add_move<gen::EVASION,side::opposit(sd)>(ml2,bd,&at2);
          auto flag = false;
          for(auto j = 0; j < ml2.size(); j++){
            if(move::is_legal(ml2.move(j),bd,at2)){
              flag = true;
              break;
            }
          }
          bd.undo();
          if(!flag) {
            is_nomate = false;
            std::cout<<"詰み見逃し\n";
            std::cout<<bd<<std::endl;
            std::cout<<move::to_can2(ml.move(i))<<std::endl;
            break;
          }
        }
      }
      return is_nomate;
    } else {
      //詰んだと言って詰んでないチェック
      if(!bd.is_ok()) {
        std::cout<<"mate1ply board is ok error\n";
        std::cout<<bd<<std::endl;
        std::cout<<move::to_can2(move)<<std::endl;
        return false;
      }
      if(!move::is_check(move,bd)){
        std::cout<<"mate1ply move is not check \n";
        std::cout<<bd<<std::endl;
        std::cout<<move::to_can2(move)<<std::endl;
        return false;
      }
      bd.move(move,true);
      gen::List ml;
      attack::Attacks at;
      attack::init_attacks(at,bd);
      gen::add_move<gen::EVASION,side::opposit(sd)>(ml,bd,&at);
      auto is_mate = true;
      for(auto i = 0; i < ml.size(); i++){
        if(move::is_legal(ml.move(i),bd,at)){
          is_mate = false;
          std::cout<<"HIT\n";
          std::cout<<move::to_can2(ml.move(i))<<std::endl;
          break;
        }
      }
      bd.undo();
      if(!is_mate){
        std::cout<<"ERROR\n";
        std::cout<<bd<<std::endl;
        std::cout<<move::to_can2(move)<<std::endl;
      }
      return is_mate;
    }
  }


  template<int sd>
    bool can_escape(const int to, board::Board &bd, const bit::Bitboard &avoid);
  template<int sd>
    bool can_capture(const int to, board::Board &bd, const bit::Bitboard &pinned);
  template<int sd, bool has_gold, bool has_bishop, bool has_rook>int mate_in_1ply(board::Board &bd, const bit::Bitboard &pinned);

  template<int sd>int mate_in_1ply(board::Board &bd, const bit::Bitboard &pinned){

    assert(bd.is_ok());
    assert(attack::is_legal(bd));
    assert(!attack::is_in_check(bd));
    auto state = 0;
    const auto h = bd.hand(sd);
    (hand::has(h,piece::GOLD))   ? state += 1 : state += 0;
    (hand::has(h,piece::BISHOP)) ? state += 2 : state += 0;
    (hand::has(h,piece::ROOK))   ? state += 4 : state += 0;
    int move;
#define ADD_DROP_MOVE(n) {\
  constexpr auto rook_flag = (n & (1 << 2)) != 0;\
  constexpr auto bishop_flag  = (n & (1 << 1)) != 0;\
  constexpr auto gold_flag   = (n & (1 << 0)) != 0;\
  move = mate_in_1ply<sd,gold_flag,bishop_flag,rook_flag>(bd,pinned); break;\
}
    switch (state) {
      case 0:
        ADD_DROP_MOVE(0)
      case 1:
          ADD_DROP_MOVE(1)
      case 2:
            ADD_DROP_MOVE(2)
      case 3:
              ADD_DROP_MOVE(3)
      case 4:
                ADD_DROP_MOVE(4)
      case 5:
                  ADD_DROP_MOVE(5)
      case 6:
                    ADD_DROP_MOVE(6)
      case 7:
                      ADD_DROP_MOVE(7)
      case 8:
                        ADD_DROP_MOVE(8)
      case 9:
                          ADD_DROP_MOVE(9)
      default:
                            assert(false);
    }
#undef ADD_DROP_MOVE
assert(bd.is_ok());
assert(is_ok<sd>(move,bd));
return move;
}
template<int sd, bool has_gold, bool has_bishop, bool has_rook>
int mate_in_1ply(board::Board &bd, const bit::Bitboard &pinned){

  assert(!attack::is_in_check(bd));

  //Drops
  constexpr auto xd = side::opposit(sd);
  bit::Bitboard bb_drop = ~bd.all();
  const auto hand = bd.hand(sd);
  const auto opp_king_sq = bd.king(xd);
  const bit::Bitboard drop_xd_pinned = attack::pinned_by(xd,bd);

  {
    if(has_rook){
      bit::Bitboard bb = attack::get_plus_attack(opp_king_sq) & bb_drop;
      int to;
      while(!bb.is_empty()){
        to = bb.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard avoid = attack::pseudo_attack_from<piece::ROOK>(sd,to);
        if(can_escape<xd>(to,bd,avoid)){ continue;}
        if(can_capture<xd>(to,bd,drop_xd_pinned)){ continue;}
        return move::make(to,piece::ROOK);
      };
    }
    if(has_bishop){
      bit::Bitboard bb = attack::get_x_attack(opp_king_sq) & bb_drop;
      int to;
      while(!bb.is_empty()){
        to = bb.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard avoid = attack::pseudo_attack_from<piece::BISHOP>(sd,to);
        if(can_escape<xd>(to,bd,avoid)){ continue;}
        if(can_capture<xd>(to,bd,drop_xd_pinned)){ continue;}
        return move::make(to,piece::BISHOP);
      };
    }
    if(has_gold){
      bit::Bitboard bb;
      if(has_rook){
        //尻金は調べなくていい
        bb = attack::get_gold_attack(xd,opp_king_sq)
          & bb_drop & (~attack::get_pawn_attack(sd,opp_king_sq));
      }else{
        bb = attack::get_gold_attack(xd,opp_king_sq) & bb_drop;
      }
      int to;
      while(!bb.is_empty()){
        to = bb.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard avoid = (attack::pseudo_attack_from<piece::GOLD>(sd,to));
        if(can_escape<xd>(to,bd,avoid)){ continue;}
        if(can_capture<xd>(to,bd,drop_xd_pinned)){ continue;}
        return move::make(to,piece::GOLD);
      };
    }
    if(hand::has(hand,piece::SILVER)){
      bit::Bitboard bb;
      if(has_bishop && has_gold){
        goto silver_next;
      }
      else if(has_bishop){
        //前3つの位置だけ調べる
        bb = attack::get_gold_attack(xd,opp_king_sq)
          & attack::get_silver_attack(xd,opp_king_sq) & bb_drop;
      }
      else if(has_gold){
        //尻銀だけ調べる
        bb = attack::get_silver_attack(xd,opp_king_sq)
          & attack::get_gold_attack(sd,opp_king_sq) & bb_drop;

      }else{
        bb = attack::get_silver_attack(xd,opp_king_sq) & bb_drop;
      }
      int to;
      while(!bb.is_empty()){
        to = bb.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard avoid = (attack::pseudo_attack_from<piece::SILVER>(sd,to));
        if(can_escape<xd>(to,bd,avoid)){ continue;}
        if(can_capture<xd>(to,bd,drop_xd_pinned)){ continue;}
        return move::make(to,piece::SILVER);
      };
    }
silver_next:
    if(hand::has(hand,piece::KNIGHT)){
      bit::Bitboard bb = attack::get_knight_attack(xd,opp_king_sq) & bb_drop;
      int to;
      while(!bb.is_empty()){
        to = bb.lsb();
        //桂馬は行き先の効きがなくても良い
        //if(!attack::is_attacked(to,sd,bd)){ continue; }
        bit::Bitboard avoid;
        avoid.init();
        if(can_escape<xd>(to,bd,avoid)){ continue;}
        if(can_capture<xd>(to,bd,drop_xd_pinned)){ continue;}
        return move::make(to,piece::KNIGHT);
      };
    }
    if(!has_rook && hand::has(hand,piece::LANCE)){
      const auto opp_king_rank = square::rank(opp_king_sq);
      if((sd == side::BLACK && opp_king_rank == square::RANK_9)
          || (sd == side::WHITE && opp_king_rank == square::RANK_1)){
        goto lance_next;
      }
      int to = (sd == side::BLACK) ? opp_king_sq + square::INC_DOWN
        : opp_king_sq + square::INC_UP ;
      if(bd.square(to)){ goto lance_next; }
      if(!attack::is_attacked(to,sd,bd)){ goto lance_next; }
      const bit::Bitboard avoid
        = attack::pseudo_attack_from<piece::LANCE>(sd,to);
      if(can_escape<xd>(to,bd,avoid)){ goto lance_next;}
      if(can_capture<xd>(to,bd,drop_xd_pinned)){ goto lance_next;}
      return move::make(to,piece::LANCE);
    }
  }
lance_next:
  //moves
  bit::Bitboard bb_move = ~bd.side(sd);
  const auto my_king_sq = bd.king(sd);
  const bit::Bitboard xd_pinned_org = attack::pinned_by(xd,bd);
  //prook
  {
    bit::Bitboard bb = bd.piece(piece::PROOK,sd);
    int from;
    while(!bb.is_empty()){
      from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_prook_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bb_check &= attack::get_king_attack(opp_king_sq);
      if(bb_check.is_empty()){ continue; }
      bd.clear_square(piece::PROOK,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check.is_empty()){
        to = bb_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid = attack::get_prook_attack(to,bd.all() ^ bit::g_mask[opp_king_sq]);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::PROOK,sd,from);
        return move::make(from,to,piece::PROOK,bd.square(to));
      };
      bd.set_square(piece::PROOK,sd,from);
    };
  }
  //rook 1-3
  {
    bit::Bitboard bb = bd.piece(piece::ROOK,sd) & bit::g_prom[sd];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_rook_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bb_check &= attack::get_king_attack(opp_king_sq);
      if(bb_check.is_empty()){ continue; }
      bd.clear_square(piece::ROOK,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check.is_empty()) {
        to = bb_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid = attack::get_prook_attack(to,bd.all() ^ bit::g_mask[opp_king_sq]);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::ROOK,sd,from);
        return move::make(from,to,piece::ROOK,bd.square(to),true);
      };
      bd.set_square(piece::ROOK,sd,from);
    }
  }
  //rook 4-9
  {
    bit::Bitboard bb = bd.piece(piece::ROOK,sd) & bit::g_middle[sd];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_rook_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bit::Bitboard bb_check13 = bb_check & attack::get_king_attack(opp_king_sq) & bit::g_prom[sd];
      bit::Bitboard bb_check49 = bb_check & attack::get_plus_attack(opp_king_sq) & bit::g_middle[sd];
      if((bb_check13 | bb_check49).is_empty()){ continue; }
      bd.clear_square(piece::ROOK,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check13.is_empty()){
        to = bb_check13.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid = attack::get_prook_attack(to,bd.all() ^ bit::g_mask[opp_king_sq]);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::ROOK,sd,from);
        return move::make(from,to,piece::ROOK,bd.square(to),true);
      }
      while(!bb_check49.is_empty()){
        to = bb_check49.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid = attack::pseudo_attack_from<piece::ROOK>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::ROOK,sd,from);
        return move::make(from,to,piece::ROOK,bd.square(to));
      }
      bd.set_square(piece::ROOK,sd,from);
    }
  }
  //pbishop
  {
    bit::Bitboard bb = bd.piece(piece::PBISHOP,sd);
    int from;
    while(!bb.is_empty()) {
      from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_pbishop_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bb_check &= attack::get_king_attack(opp_king_sq);
      if(bb_check.is_empty()){ continue; }
      bd.clear_square(piece::PBISHOP,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check.is_empty()) {
        to = bb_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::PBISHOP>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::PBISHOP,sd,from);
        return move::make(from,to,piece::PBISHOP,bd.square(to));
      };
      bd.set_square(piece::PBISHOP,sd,from);
    };
  }
  //bishop 1-3
  {
    bit::Bitboard bb = bd.piece(piece::BISHOP,sd) & bit::g_prom[sd];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_bishop_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bb_check &= attack::get_king_attack(opp_king_sq);
      if(bb_check.is_empty()){ continue; }
      bd.clear_square(piece::BISHOP,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check.is_empty()) {
        to = bb_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::PBISHOP>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::BISHOP,sd,from);
        return move::make(from,to,piece::BISHOP,bd.square(to),true);
      };
      bd.set_square(piece::BISHOP,sd,from);
    }
  }
  //bishop 4-9
  {
    bit::Bitboard bb = bd.piece(piece::BISHOP,sd) & bit::g_middle[sd];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_bishop_attack(from,bd.all());
      bit::Bitboard bb_check = bb_attacks & bb_move;
      auto bb_check13 = bb_check & attack::get_king_attack(opp_king_sq) & bit::g_prom[sd];
      auto bb_check49 = bb_check & attack::get_x_attack(opp_king_sq) & bit::g_middle[sd];
      if((bb_check13 | bb_check49).is_empty()){ continue; }
      bd.clear_square(piece::BISHOP,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      while(!bb_check13.is_empty()){
        const auto to = bb_check13.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::PBISHOP>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::BISHOP,sd,from);
        return move::make(from,to,piece::BISHOP,bd.square(to),true);
      }
      while(!bb_check49.is_empty()){
        const auto to = bb_check49.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::BISHOP>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::BISHOP,sd,from);
        return move::make(from,to,piece::BISHOP,bd.square(to));
      }
      bd.set_square(piece::BISHOP,sd,from);
    }
  }
  //golds
  {
    //TODO separate up and down
    bit::Bitboard bb = bd.golds(sd) & attack::g_gold_check_table[sd][opp_king_sq];
    int from;
    while(!bb.is_empty()) {
      from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_gold_attack(sd,from);
      bit::Bitboard bb_check = bb_attacks & bb_move;
      bb_check &= attack::get_gold_attack(xd,opp_king_sq);
      if(bb_check.is_empty()){ continue; }
      const auto pc = bd.square(from);
      bd.clear_square(pc,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      int to;
      while(!bb_check.is_empty()) {
        to = bb_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::GOLD>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(pc,sd,from);
        return move::make(from,to,pc,bd.square(to));
      }
      bd.set_square(pc,sd,from);
    }
  }
  //silver 1-3
  {
    bit::Bitboard bb
      = bd.piece(piece::SILVER,sd) & attack::g_silver_check_table[sd][opp_king_sq] & bit::g_prom[sd];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const auto attacks = attack::get_silver_attack(sd,from);
      auto check = attacks & bb_move;
      auto npm_check = check & attack::get_silver_attack(xd,opp_king_sq);
      auto pm_check = check & attack::get_gold_attack(xd,opp_king_sq);
      if((npm_check | pm_check).is_empty()){ continue; }
      //TODO
      bd.clear_square(piece::SILVER,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      //prom
      while(!pm_check.is_empty()){
        const auto to = pm_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::GOLD>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::SILVER,sd,from);
        return move::make(from,to,piece::SILVER,bd.square(to),true);
      }
      //bd.set_square(piece::SILVER,sd,from);
      //王の前に行っても絶対に詰まない
      npm_check &= ~bit::g_rank_mask[square::rank(opp_king_sq) + ((sd == side::BLACK) ? square::INC_DOWN : square::INC_UP)];
      //noprom
      while(!npm_check.is_empty()){
        const auto to = npm_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::SILVER>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない(
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::SILVER,sd,from);
        return move::make(from,to,piece::SILVER,bd.square(to));
      }
      bd.set_square(piece::SILVER,sd,from);
    }
  }
  //silver 4
  {
    bit::Bitboard bb
      = bd.piece(piece::SILVER,sd) & attack::g_silver_check_table[sd][opp_king_sq] & bit::g_rank_mask[(sd== side::BLACK) ? square::RANK_4 : square::RANK_6];
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const auto attacks = attack::get_silver_attack(sd,from);
      auto check = attacks & bb_move;
      auto npm_check = check & attack::get_silver_attack(xd,opp_king_sq);
      auto pm_check = check & attack::get_gold_attack(xd,opp_king_sq) & bit::g_prom[sd];
      if((npm_check | pm_check).is_empty()){ continue; }
      bd.clear_square(piece::SILVER,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      //prom
      while(!pm_check.is_empty()) {
        const auto to = pm_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::GOLD>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::SILVER,sd,from);
        return move::make(from,to,piece::SILVER,bd.square(to),true);
      }
      //bd.set_square(piece::SILVER,sd,from);
      //noprom
      //bd.set_square(piece::SILVER,sd,from);
      while(!npm_check.is_empty()) {
        const auto to = npm_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::SILVER>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::SILVER,sd,from);
        return move::make(from,to,piece::SILVER,bd.square(to));
      }
      bd.set_square(piece::SILVER,sd,from);
    }
  }
  //silver 5-9
  {
    bit::Bitboard bb
      = bd.piece(piece::SILVER,sd) & attack::g_silver_check_table[sd][opp_king_sq]
      & bit::g_middle[sd] & ~(bit::g_rank_mask[(sd== side::BLACK) ? square::RANK_4 : square::RANK_6]);
    while(!bb.is_empty()) {
      const auto from = bb.lsb();
      const auto attacks = attack::get_silver_attack(sd,from);
      auto check = attacks & bb_move & attack::get_silver_attack(xd,opp_king_sq);
      if(check.is_empty()){ continue; }
      bd.clear_square(piece::SILVER,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      while(!check.is_empty()){
        const auto to = check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::SILVER>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::SILVER,sd,from);
        return move::make(from,to,piece::SILVER,bd.square(to));
      }
      bd.set_square(piece::SILVER,sd,from);
    }
  }
  //knight
  {
    bit::Bitboard bb
      = bd.piece(piece::KNIGHT,sd) & attack::g_knight_check_table[sd][opp_king_sq];
    int from;
    while(!bb.is_empty()) {
      from = bb.lsb();
      const auto attacks = attack::get_knight_attack(sd,from) & bb_move;
      bit::Bitboard gld_check = attacks
        & attack::get_gold_attack(xd,opp_king_sq) & bit::g_prom[sd];
      const auto rank = (sd == side::BLACK) ? square::RANK_3 : square::RANK_7;
      bit::Bitboard knt_check = attacks
        & attack::get_knight_attack(xd,opp_king_sq)
        & (bit::g_middle[sd] | bit::g_rank_mask[rank]);
      if((gld_check | knt_check).is_empty()){ continue; }
      bd.clear_square(piece::KNIGHT,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      //prom
      while(!gld_check.is_empty()){
        const auto to = gld_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::GOLD>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        //桂馬は飛び効きのある方向へは行けないので、ピンかどうかだけ判定する
        if(pinned.is_set(from)){ continue; }
        bd.set_square(piece::KNIGHT,sd,from);
        return move::make(from,to,piece::KNIGHT,bd.square(to),true);
      }
      //bd.set_square(piece::KNIGHT,sd,from);
      //TODO
      //bd.clear_square(piece::KNIGHT,sd,from);
      //noprom
      while(!knt_check.is_empty()){
        const auto to = knt_check.lsb();
        //桂馬は行き先の効きがなくても良い
        bit::Bitboard bb_avoid;
        bb_avoid.init();
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        //桂馬は飛び効きのある方向へは行けないので、ピンかどうかだけ判定する
        if(pinned.is_set(from)){ continue; }
        bd.set_square(piece::KNIGHT,sd,from);
        //noprom
        return move::make(from,to,piece::KNIGHT,bd.square(to));
      }
      bd.set_square(piece::KNIGHT,sd,from);
    };
  }
  //lance
  {
    bit::Bitboard bb
      = bd.piece(piece::LANCE,sd) & attack::g_lance_check_table[sd][opp_king_sq];
    int from;
    while(!bb.is_empty()) {
      from = bb.lsb();
      const bit::Bitboard bb_attacks
        = attack::get_lance_attack(sd,from,bd.all()) & bb_move;
      bit::Bitboard gld_check = bb_attacks
        & attack::get_gold_attack(xd,opp_king_sq) & bit::g_prom[sd];
      const auto rank = (sd == side::BLACK) ? square::RANK_3 : square::RANK_7;
      bit::Bitboard lnc_check = bb_attacks
        & attack::get_pawn_attack(xd,opp_king_sq)
        & (bit::g_middle[sd] | bit::g_rank_mask[rank]);
      if((gld_check | lnc_check).is_empty()){ continue; }
      bd.clear_square(piece::LANCE,sd,from);
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      //prom
      while(!gld_check.is_empty()){
        const auto to = gld_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::GOLD>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::LANCE,sd,from);
        return move::make(from,to,piece::LANCE,bd.square(to),true);
      }
      //bd.clear_square_gold(piece::LANCE,sd,from);
      //noprom
      while(!lnc_check.is_empty()){
        const auto to = lnc_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = (attack::pseudo_attack_from<piece::LANCE>(sd,to));
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::LANCE,sd,from);
        return move::make(from,to,piece::LANCE,bd.square(to));
      }
      bd.set_square(piece::LANCE,sd,from);
    };
  }
  //pawn
  {
    bit::Bitboard bb
      = bd.piece(piece::PAWN,sd) & attack::g_pawn_check_table[sd][opp_king_sq];
    int from;
    while(!bb.is_empty()) {
      from = bb.lsb();
      const bit::Bitboard bb_attacks = attack::get_pawn_attack(sd,from) & bb_move;
      bit::Bitboard gld_check = bb_attacks & attack::get_gold_attack(xd,opp_king_sq) & bit::g_prom[sd];
      bit::Bitboard pwn_check = bb_attacks & attack::get_pawn_attack(xd,opp_king_sq) & bit::g_middle[sd];
      if((gld_check | pwn_check).is_empty()){ continue; }
      const bit::Bitboard xd_pinned = attack::pinned_by(xd,bd);
      bd.clear_square(piece::PAWN,sd,from);
      //prom
      while(!gld_check.is_empty()){
        const auto to = gld_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ continue; }
        const bit::Bitboard bb_avoid
          = attack::pseudo_attack_from<piece::GOLD>(sd,to);
        if(can_escape<xd>(to,bd,bb_avoid)){ continue; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ continue; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::PAWN,sd,from);
        return move::make(from,to,piece::PAWN,bd.square(to),true);
      }
      //bd.set_square_gold(piece::PAWN,sd,from);
      //TODO
      //bd.clear_square(piece::PAWN,sd,from);
      //noprom
      while(!pwn_check.is_empty()){
        const auto to = pwn_check.lsb();
        if(!attack::is_attacked(to,sd,bd)){ break; }
        bit::Bitboard bb_avoid;
        bb_avoid.init();
        if(can_escape<xd>(to,bd,bb_avoid)){ break; }
        if(move::is_discover(from,to,opp_king_sq,xd_pinned_org)){ ; }//空き王手は王手している駒をとって王手回避ができない
        else if(can_capture<xd>(to,bd,xd_pinned)){ break; }
        if(move::is_discover(from,to,my_king_sq,pinned)){ continue; }//王手は合法手かどうか確認
        bd.set_square(piece::PAWN,sd,from);
        return move::make(from,to,piece::PAWN,bd.square(to));
      }
      bd.set_square(piece::PAWN,sd,from);
    };
  }
  return move::NONE;
}
//sdは玉方の手番
template<int sd>
bool can_escape(const int to, board::Board &bd, const bit::Bitboard &avoid){
  const auto king_sq = bd.king(sd);
  constexpr auto xd = side::opposit(sd);
  bit::Bitboard bb = avoid;
  assert((!bb.is_set(to)));
  bb.set(to);
  bb = (~bb) & (~bd.side(sd));
  //行き先を決定する
  bb &= attack::get_king_attack(king_sq);
  if(bb.is_empty()) {
    return false;
  }
  bit::Bitboard occ = bd.all();
  occ |= to;
  occ &= ~(bit::g_mask[king_sq]);
  do {
    const auto escape = bb.lsb();
    if(!attack::is_attacked(escape,xd,bd,occ)){
      return true;
    }
  } while(!bb.is_empty());
  return false;
}
//sdは玉方の手番
template<int sd>
bool can_capture(const int to, board::Board &bd, const bit::Bitboard &pinned){
  bit::Bitboard bb_sum = attack::attack_to<false,true>(sd,to,bd);
  int from;
  while(!bb_sum.is_empty()) {
    from = bb_sum.lsb();
    if(!move::is_discover(from,to,bd.king(sd),pinned)){
      return true;
    }
  };
  return false;
}
}


namespace search {

  constexpr auto MAX_DEPTH = 100;
  constexpr auto MAX_PLY = 100;
  constexpr auto NODE_PERIOD = 1024;

  constexpr auto MAX_THREADS = 16;

  class Abort: public std::exception { // SP fail-high exception

  };

  void update_current();

  class PV {

    private:

      static constexpr int SIZE = MAX_PLY;

      int size_;
      int move_[SIZE];

    public:

      PV() {
        clear();
      }

      void operator=(const PV & pv) {
        clear();
        add(pv);
      }

      void clear() {
        size_ = 0;
      }

      void add(int mv) {
        if (size_ < SIZE) {
          move_[size_++] = mv;
        }
      }

      void add(const PV & pv) {
        for (auto pos = 0; pos < pv.size(); ++pos) {
          add(pv.move(pos));
        }
      }

      void cat(int mv, const PV & pv) {
        clear();
        add(mv);
        add(pv);
      }

      int size() const {
        return size_;
      }

      int move(int pos) const {
        return move_[pos];
      }

      std::string to_can() const {

        std::string s;

        for (auto pos = 0; pos < size(); ++pos) {
          auto mv = move(pos);
          if (pos != 0)
            s += " ";
          s += move::to_can(mv);
        }

        return s;
      }

  };

  struct Time {
    bool depth_limited;
    bool node_limited;
    bool time_limited;
    int depth_limit;
    int64 node_limit;
    int64 time_limit;
    bool smart;
    bool ponder;
    bool flag;
    int64 limit_0;
    int64 limit_1;
    int64 limit_2;
    int last_score;
    bool drop;
    util::Timer timer;
  };

  struct Current {

    int depth;
    int max_ply;
    int64 node;
    int time;
    int speed;

    int move;
    int pos;
    int size;
    bool fail_high;

    int last_time;
  };

  struct Best {
    int depth;
    int move;
    int score;
    int flags;
    PV pv;
  };

  Time g_time;
  Current g_current;
  Best g_best;

  class SearchGlobal: public util::Lockable {

    public:

      trans::Table trans;
      sort::History history;

  };

  SearchGlobal g_sg;

  class SMP: public util::Lockable {

  };

  SMP smp;

  class SearchLocal;
  class SplitPoint;

  //void clear_iteration(SearchLocal & sl);
  template<int sd>void search_root(SearchLocal & sl, gen::List & ml, int depth, int alpha, int beta);
  template<int sd>int search(SearchLocal & sl, int depth, int alpha, int beta, PV & pv);
  //int split(SearchLocal & sl, int depth, int old_alpha, int alpha, int beta, PV & pv, gensort::List & todo, const gen::List & done, int bs, int bm);
  template<int sd>void master_split_point(SearchLocal & sl, SplitPoint & sp);
  void search_split_point(SearchLocal & sl, SplitPoint & sp);
  template<int sd>int qs_static(SearchLocal & sl, int beta, int gain);
  void inc_node(SearchLocal & sl);
  bool poll();
  void move(SearchLocal & sl, const int mv, const bool check);
  void undo(SearchLocal & sl);
  int eval(SearchLocal & sl);
  int extension(SearchLocal & sl, int mv, int depth, bool pv_node, bool check);
  int reduction(SearchLocal & sl, int mv, int depth, bool pv_node,
      bool in_check, int searched_size, bool dangerous);
  int limit_extension(const int ext, const int ply);
  void gen_sort(SearchLocal & sl, gen::List & ml);

  void sg_abort();

  void sl_init_early(SearchLocal & sl, int id);
  void sl_init_late(SearchLocal & sl);
  void sl_set_root(SearchLocal & sl, const board::Board & bd);
  void sl_signal(SearchLocal & sl);
  bool sl_stop(const SearchLocal & sl);
  bool sl_idle(const SearchLocal & sl, SplitPoint * sp);
  void sl_push(SearchLocal & sl, SplitPoint & sp);
  void sl_pop(SearchLocal & sl);
  SplitPoint & sl_top(const SearchLocal & sl);

  class SplitPoint: public util::Lockable {

    private:

      SearchLocal * master_;
      SplitPoint * parent_;

      board::Board board_;
      int depth_;
      int old_alpha_;
      int volatile alpha_;
      int beta_;

      gen::List todo_;
      gen::List done_;

      int volatile workders_;
      int volatile sent_;
      int volatile received_;

      int volatile bs_;
      int volatile bm_;
      PV pv_;

    public:

      void init_root(SearchLocal & master) {

        master_ = &master;
        parent_ = nullptr;

        bs_ = score::NONE;
        beta_ = score::MAX;
        todo_.clear();

        workders_ = 1;
        received_ = -1; // HACK
      }

      //	 void init(SearchLocal & master, SplitPoint & parent, const board::Board & bd, int depth, int old_alpha, int alpha, int beta, gensort::List & todo, const gen::List & done, int bs, int bm, const PV & pv) {
      //
      //	 assert(depth > 4);
      //	 assert(old_alpha <= alpha);
      //	 assert(alpha < beta);
      //	 assert(done.size() != 0);
      //	 assert(bs != score::NONE);
      //
      //	 master_ = &master;
      //	 parent_ = &parent;
      //
      //	 board_ = bd;
      //	 depth_ = depth;
      //	 old_alpha_ = old_alpha;
      //	 alpha_ = alpha;
      //	 beta_ = beta;
      //
      //	 todo_.clear();
      //
      //	 for (int mv = todo.next(); mv != move::NONE; mv = todo.next()) {
      //		 todo_.add(mv);
      //	 }
      //
      //	 done_ = done;
      //
      //	 workders_ = 0;
      //	 sent_ = 0;
      //	 received_ = 0;
      //
      //	 bs_ = bs;
      //	 bm_ = bm;
      //	 pv_ = pv;
      //	 }

      void enter() {
        lock();
        workders_++;
        unlock();
      }

      void leave() {

        lock();

        assert(workders_ > 0);
        workders_--;

        if (workders_ == 0)
          sl_signal(*master_);

        unlock();
      }

      int next_move() {

        // lock();

        int mv = move::NONE;

        if (bs_ < beta_ && sent_ < todo_.size()) {
          mv = todo_.move(sent_++);
        }

        // unlock();

        return mv;
      }

      void update_root() {
        lock();
        received_ = 0;
        workders_ = 0;
        unlock();
      }

      void update(int mv, int sc, const PV & pv) {

        lock();

        done_.add(mv);

        assert(received_ < todo_.size());
        received_++;

        if (sc > bs_) {

          bs_ = sc;
          pv_.cat(mv, pv);

          if (sc > alpha_) {
            bm_ = mv;
            alpha_ = sc;
          }
        }

        unlock();
      }

      const board::Board & board() const {
        return board_;
      }

      SplitPoint * parent() const {
        return parent_;
      }

      int depth() const {
        return depth_;
      }
      int alpha() const {
        return alpha_;
      }
      int beta() const {
        return beta_;
      }
      int old_alpha() const {
        return old_alpha_;
      }
      int bs() const {
        return bs_;
      }
      int bm() const {
        return bm_;
      }
      bool solved() const {
        return bs_ >= beta_ || received_ == todo_.size();
      }
      bool free() const {
        return workders_ == 0;
      }

      const gen::List & searched() const {
        return done_;
      }
      int searched_size() const {
        return done_.size();
      }

      int result(PV & pv) const {
        pv = pv_;
        return bs_;
      }

  };

  SplitPoint g_root_sp;

  class SearchLocal: public util::Waitable {

    public:

      int id;
      std::thread thread;

      bool volatile todo;
      SplitPoint * volatile todo_sp;

      board::Board board;
      sort::Killer killer;
      //pawn::Table pawn_table;
      eval::Table eval_table;

      int64 volatile node;
      int volatile max_ply;

      SplitPoint msp_stack[16];
      int msp_stack_size;

      SplitPoint * ssp_stack[64];
      int ssp_stack_size;
  };

  SearchLocal g_sl[MAX_THREADS];

  void new_search() {

    g_time.depth_limited = true;
    g_time.node_limited = false;
    g_time.time_limited = false;

    g_time.depth_limit = MAX_DEPTH - 1;

    g_time.smart = false;
    g_time.ponder = false;
  }

  void set_depth_limit(int depth) {
    g_time.depth_limited = true;
    g_time.depth_limit = depth;
  }

  void set_node_limit(int64 node) {
    g_time.node_limited = true;
    g_time.node_limit = node;
  }

  void set_time_limit(int64 time) {
    g_time.time_limited = true;
    g_time.time_limit = time;
  }

  void set_ponder() {
    g_time.ponder = true;
  }

  void clear() {
    g_time.flag = false;
    g_time.timer.reset();
    g_time.timer.start();

    g_current.depth = 0;
    g_current.max_ply = 0;
    g_current.node = 0;
    g_current.time = 0;
    g_current.speed = 0;

    g_current.move = move::NONE;
    g_current.pos = 0;
    g_current.size = 0;
    g_current.fail_high = false;

    g_current.last_time = 0;

    g_best.depth = 0;
    g_best.move = move::NONE;
    g_best.score = score::NONE;
    g_best.flags = score::FLAGS_NONE;
    g_best.pv.clear();
  }

  void write_pv(Best & best) {

    g_sg.lock();

    util::Tee << "info";
    util::Tee << " depth " << g_best.depth;
    util::Tee << " seldepth " << g_current.max_ply;
    util::Tee << " nodes " << g_current.node;
    util::Tee << " time " << g_current.time;

    if (score::is_mate(best.score)) {
      util::Tee << " score mate " << score::signed_mate(best.score);
    } else {
      util::Tee << " score cp " << best.score;
    }
    if (best.flags == score::FLAGS_LOWER) util::Tee << " lowerbound";
    if (best.flags == score::FLAGS_UPPER) util::Tee << " upperbound";

    util::Tee << " pv " << best.pv.to_can();
    util::Tee << std::endl;

    g_sg.unlock();
  }

  void write_info() {

    g_sg.lock();

    util::Tee << "info";
    util::Tee << " depth " << g_current.depth;
    util::Tee << " seldepth " << g_current.max_ply;
    util::Tee << " currmove " << move::to_can(g_current.move);
    util::Tee << " currmovenumber " << g_current.pos + 1;
    util::Tee << " nodes " << g_current.node;
    util::Tee << " time " << g_current.time;
    if (g_current.speed != 0) util::Tee << " nps " << g_current.speed;
    util::Tee << " hashfull " << g_sg.trans.used();
    util::Tee << std::endl;

    g_sg.unlock();
  }

  void write_info_opt() {

    int time = g_current.time;

    if (time >= g_current.last_time + 1000) {
      write_info();
      g_current.last_time = time - time % 1000;
    }
  }

  void update_current() {

    int64 node = 0;
    int max_ply = 0;

    for (int id = 0; id < engine::g_engine.threads; id++) {

      SearchLocal & sl = g_sl[id];

      node += sl.node;
      if (sl.max_ply > max_ply) max_ply = sl.max_ply;
    }

    g_current.node = node;
    g_current.max_ply = max_ply;

    g_current.time = g_time.timer.elapsed();
    g_current.speed = (g_current.time < 10) ? 0 : int(g_current.node * 1000 / g_current.time);
  }

  void search_end() {
    g_time.timer.stop();
    update_current();
    write_info();
  }

  void update_best(Best & best, int sc, int flags, const PV & pv) {

    assert(sc != score::NONE);
    assert(pv.size() != 0);

    g_time.drop = flags == score::FLAGS_UPPER || (sc <= g_time.last_score - 30 && g_current.size > 1);

    if (pv.move(0) != best.move || g_time.drop) {
      g_time.flag = false;
    }

    best.depth = g_current.depth;
    best.move = pv.move(0);
    best.score = sc;
    best.flags = flags;
    best.pv = pv;
  }

  void idle_loop(SearchLocal & /*sl*/, SplitPoint & /*wait_sp*/) {

  }

  void helper_program(SearchLocal * sl) {
    sl_init_late(*sl);
    idle_loop(*sl, g_root_sp);
  }

  void init_sg() {
    g_sg.history.clear();
  }


  void depth_start(int depth) {
    g_current.depth = depth;
  }

  void depth_end() {
  }

  void move_start(int mv, int pos, int size) {

    assert(size > 0);
    assert(pos < size);

    g_current.move = mv;
    g_current.pos = pos;
    g_current.size = size;

    g_current.fail_high = false;
  }

  void move_fail_high() {
    g_current.fail_high = true;
    g_time.flag = false;
  }

  void move_end() {
    g_current.fail_high = false;
  }
  template<int sd>
    void search_root(SearchLocal & sl, gen::List & ml, int depth, int alpha, int beta) {

      assert(depth > 0 && depth < MAX_DEPTH);
      assert(alpha < beta);

      board::Board & bd = sl.board;
      assert(attack::is_legal(bd));

      bool pv_node = true;

      int bs = score::NONE;
      int bm = move::NONE;
      int old_alpha = alpha;

      // move loop

      bool in_check = attack::is_in_check(bd);

      int searched_size = 0;

      for (int pos = 0; pos < ml.size(); pos++) {

        int mv = ml.move(pos);

        const auto is_check = move::is_check(mv, bd);

        bool dangerous = in_check || move::is_tactical(mv) || is_check;

        int ext = extension(sl, mv, depth, pv_node,is_check);
        int red = reduction(sl, mv, depth, pv_node, in_check, searched_size, dangerous); // LMR

        if (ext != 0) red = 0;
        assert(ext == 0 || red == 0);

        int sc;
        PV npv;
#ifdef TRACE
        util::Tee<<"ply:1:"<<move::to_can2(mv)<<std::endl;
#endif
        move_start(mv, pos, ml.size());

        move(sl, mv, is_check);

        if ((pv_node && searched_size != 0) || red != 0) {

          sc = -search<side::opposit(sd)>(sl, depth + ext - red - 1, -alpha - 1, -alpha, npv);

          if (sc > alpha) { // PVS/LMR re-search
            move_fail_high();
            sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
          }

        } else {

          sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
        }
        //sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -score::MAX, score::MAX, npv);

        //      std::cout<<"score:"<<sc<<std::endl;
        //      std::cout<<move::to_can(mv)<<" ";
        //      std::cout<<npv.to_can()<<std::endl;

        undo(sl);

        move_end();

        searched_size++;

        if (sc > bs) {

          bs = sc;

          PV pv;
          pv.cat(mv, npv);

          update_best(g_best, sc, score::flags(sc, alpha, beta), pv);

          update_current();
          write_pv(g_best);

          if (sc > alpha) {

            bm = mv;
            alpha = sc;

            // ml.set_score(pos, sc); // not needed
            ml.move_to_front(pos);

            if (depth >= 0) {
              g_sg.trans.store(bd, depth, bd.ply(), mv, sc, score::FLAGS_LOWER);
            }

            if (sc >= beta) return;
          }
        }
      }

      assert(bs != score::NONE);
      assert(bs < beta);

      if (depth >= 0) {
        int flags = score::flags(bs, old_alpha, beta);
        g_sg.trans.store(bd, depth, bd.ply(), bm, bs, flags);
      }
    }

  template<int sd>
    int search(SearchLocal & sl, int depth, int alpha, int beta, PV & pv) {

      assert(depth < MAX_DEPTH);
      assert(alpha < beta);

      board::Board & bd = sl.board;
      assert(bd.is_ok());
      assert(attack::is_legal(bd));
      assert(sd == bd.turn());

      pv.clear();

      bool pv_node = depth > 0 && beta != alpha + 1;

      // mate-distance pruning

      {
        int sc = score::from_trans(score::MATE - 1, bd.ply());

        if (sc < beta) {

          beta = sc;

          if (sc <= alpha) {
            return sc;
          }
        }
      }

      // transposition table

      switch(bd.is_draw()) {
        case score::REP_CHECK:
#ifdef TRACE
          std::cout<<"check:"<<bd<<std::endl;
#endif
          return score::SUPERIOR_MIN;
        case score::REP_CHECKED:
#ifdef TRACE
          std::cout<<"checked:"<<bd<<std::endl;
#endif
          return score::SUPERIOR_MAX;
        case score::REP_EQUAL:
#ifdef TRACE
          std::cout<<"eq:"<<bd<<std::endl;
#endif
          return 0;
        case score::REP_LOSE:
#ifdef TRACE
          std::cout<<"lose:"<<bd<<std::endl;
#endif
          return score::REP_MIN;
        case score::REP_UNKNOWN:
          break;
        case score::REP_WIN:
#ifdef TRACE
          std::cout<<"win:"<<bd<<std::endl;
#endif
          return score::REP_MAX;
      }

      attack::Attacks attacks;
      attack::init_attacks(attacks, bd);
      bool in_check = attacks.size != 0;

      bool use_trans = depth >= 0;
      int trans_depth = depth;

      if (depth < 0 && in_check) {
        use_trans = true;
        trans_depth = 0;
      }

      int trans_move = move::NONE;

      if (use_trans) {

        int score;
        int flags;

        if (g_sg.trans.retrieve(bd, trans_depth, bd.ply(), trans_move, score, flags) && !pv_node) { // assigns trans_move #
          if (flags == score::FLAGS_LOWER && score >= beta) {
            return score;
          }
          if (flags == score::FLAGS_UPPER && score <= alpha){
            return score;
          }
          if (flags == score::FLAGS_EXACT) {
            return score;
          }
        }
      }

      if(!in_check) {
        const auto move = mate1::mate_in_1ply<sd>(bd,attacks.pinned);
        if(move != move::NONE) {
          return +score::MATE - bd.ply() -1;
        }
      }
      // ply limit

      if (bd.ply() >= MAX_PLY) return eval(sl);


      // beta pruning

      if (!pv_node && depth > 0 && depth <= 3 && !score::is_mate(beta) && !in_check) {

        int sc = eval(sl) - depth * 50;

        if (sc >= beta) {
          return sc;
        }
      }

      // null-move pruning

      if (!pv_node && depth > 0 && !score::is_mate(beta) && !in_check && eval(sl) >= beta) {

        bd.move_null(); // TODO: use sl?

        int sc = score::MIN;

        if (/*depth <= 3*/false) { // static
          sc = -qs_static<side::opposit(sd)>(sl, -beta + 1, 100);
        } else { // dynamic
          PV npv;
          sc = -search<side::opposit(sd)>(sl, depth - 3 - 1, -beta, -beta + 1, npv);
        }

        bd.undo_null(); // TODO: use sl?

        if (sc >= beta) {

          if (use_trans) {
            g_sg.trans.store(bd, trans_depth, bd.ply(), move::NONE, sc, score::FLAGS_LOWER);
          }

          return sc;
        }
      }

      // stand pat

      int bs = score::NONE;
      int bm = move::NONE;
      int old_alpha = alpha;
      int val = score::NONE; // for delta pruning

      if (depth <= 0 && !in_check) {

        bs = eval(sl);
        val = bs + 100; // QS-DP margin

        if (bs > alpha) {

          alpha = bs;

          if (bs >= beta) {
            return bs;
          }
        }
      }

      // futility-pruning condition

      bool use_fp = false;

      if (depth > 0 && depth <= 8 && !score::is_mate(alpha) && !in_check) {

        int sc = eval(sl) + depth * 40;
        val = sc + 50; // FP-DP margin, extra 50 for captures

        if (sc <= alpha) {
          bs = sc;
          use_fp = true;
        }
      }

      if (depth <= 0 && !in_check) { // unify FP and QS
        use_fp = true;
      }

      // IID

      if (pv_node && depth >= 3 && trans_move == move::NONE) {

        PV npv;
        int sc = search<sd>(sl, depth - 2, alpha, beta, npv); // to keep PV-node property

        if (sc > alpha && npv.size() != 0) {
          trans_move = npv.move(0);
        }
      }

      // move loop

      gensort::List ml;
      ml.init(depth, bd, attacks, trans_move, sl.killer, g_sg.history);

      gen::List searched;

      for (int mv = ml.next<sd>(); mv != move::NONE; mv = ml.next<sd>()) {

#ifdef TRACE
        util::Tee<<"ply:"<<bd.ply()<<":"<<move::to_can2(mv)<<std::endl;
#endif

        const auto is_check = move::is_check(mv, bd);

        bool dangerous = in_check || move::is_tactical(mv) || is_check || ml.is_candidate();

        if (use_fp && move::is_tactical(mv) && !is_check && val + move::see_max(mv) <= alpha) { // delta pruning
          continue;
        }

        if (use_fp && !move::is_safe(mv, bd)) { // SEE pruning
          continue;
        }

        if (!pv_node && depth > 0 && depth <= 3 && !score::is_mate(bs) && searched.size() >= depth * 4 && !dangerous) { // late-move pruning
          continue;
        }

        int ext = extension(sl, mv, depth, pv_node, is_check);
        int red = reduction(sl, mv, depth, pv_node, in_check, searched.size(), dangerous); // LMR

        if (ext != 0) red = 0;
        assert(ext == 0 || red == 0);

        ext = limit_extension(ext,bd.ply());

        int sc;
        PV npv;

        move(sl, mv, is_check);

        if ((pv_node && searched.size() != 0) || red != 0) {

          sc = -search<side::opposit(sd)>(sl, depth + ext - red - 1, -alpha - 1, -alpha, npv);

          if (sc > alpha) { // PVS/LMR re-search
            sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
          }

        } else {

          sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
        }

        undo(sl);

        searched.add(mv);

        if (sc > bs) {

          bs = sc;
          pv.cat(mv, npv);

          if (sc > alpha) {

            bm = mv;
            alpha = sc;

            if (use_trans) {
              g_sg.trans.store(bd, trans_depth, bd.ply(), mv, sc, score::FLAGS_LOWER);
            }

            if (sc >= beta) {

              if (depth > 0 && !in_check && !move::is_tactical(mv)) {
                sl.killer.add(mv, bd.ply());
                g_sg.history.add(mv, searched, bd);
              }
#ifdef TRACE
              util::Tee<<"ply:"<<bd.ply()<<": beta cut :"<<move::to_can2(mv)<<std::endl;
#endif
              //              if(trans_flags == score::FLAGS_UPPER) {
              //            	  util::Tee<<"low\n";
              //            	  util::Tee<<bd<<std::endl;
              //            	  util::Tee<<sc<<std::endl;
              //            	  util::Tee<<trans_score<<std::endl;
              //            	  util::Tee<<beta<<std::endl;
              //            	  assert(false);
              //            	  exit(1);
              //              }
              return sc;
            }
          }
        }

        /*if (depth >= 6 && !in_check && !use_fp && can_split(sl, sl_top(sl))) {
          return split(sl, depth, old_alpha, alpha, beta, pv, ml, searched, bs, bm);
          }*/
      }

#ifdef TRACE
      util::Tee<<"ply:"<<bd.ply()<<": search end "<<std::endl;
#endif

      if (bs == score::NONE) {
        //assert(depth > 0 || in_check);
        assert(gen::legal_move_num(bd) == 0);
#ifdef TRACE
        util::Tee<<"ply:"<<bd.ply()<<": mate :"<<std::endl;
#endif
        return  -score::MATE + bd.ply();
      }

      assert(bs < beta);

      if (use_trans) {
        int flags = score::flags(bs, old_alpha, beta);
        //      if(trans_flags == score::FLAGS_LOWER) {
        //    		  util::Tee<<"ex\n";
        //    		  util::Tee<<bd<<std::endl;
        //        	  util::Tee<<bs<<std::endl;
        //        	  util::Tee<<trans_score<<std::endl;
        //        	  util::Tee<<alpha<<std::endl;
        //        	  util::Tee<<beta<<std::endl;
        //    		  assert(false);
        //    		  exit(1);
        //
        //      }
        g_sg.trans.store(bd, trans_depth, bd.ply(), bm, bs, flags);
      }

#ifdef TRACE
      util::Tee<<"ply:"<<bd.ply()<<": search end :"<<bs<<std::endl;
#endif

      return bs;
    }

  //int split(Search_Local & master, int depth, int old_alpha, int alpha, int beta, PV & pv, gen_sort::List & todo, const gen::List & done, int bs, int bm) {
  //
  //   smp.lock();
  //
  //   assert(master.msp_stack_size < 16);
  //   Split_Point & sp = master.msp_stack[master.msp_stack_size++];
  //
  //   Split_Point & parent = sl_top(master);
  //
  //   sp.init(master, parent, master.board, depth, old_alpha, alpha, beta, todo, done, bs, bm, pv);
  //
  //   for (int id = 0; id < engine::engine.threads; id++) {
  //
  //      Search_Local & worker = p_sl[id];
  //
  //      if (&worker != &master && sl_idle(worker, &parent)) {
  //         send_work(worker, sp);
  //      }
  //   }
  //
  //   smp.unlock();
  //
  //   try {
  //      master_split_point(master, sp);
  //   } catch (const Abort & /* abort */) {
  //      // no-op
  //   }
  //
  //   assert(master.msp_stack_size > 0);
  //   assert(&master.msp_stack[master.msp_stack_size - 1] == &sp);
  //   master.msp_stack_size--;
  //
  //   return sp.result(pv);
  //}

  template<int sd>void search_split_point(SearchLocal & sl, SplitPoint & sp) {

    board::Board & bd = sl.board;
    bd = sp.board();

    int depth = sp.depth();
    int old_alpha = sp.old_alpha();
    int beta = sp.beta();

    bool pv_node = depth > 0 && beta != old_alpha + 1;

    bool in_check = attack::is_in_check(bd);

    while (true) {

      sp.lock();
      int mv = sp.next_move();
      int alpha = sp.alpha();
      int searched_size = sp.searched_size();
      sp.unlock();

      if (mv == move::NONE) {
        break;
      }

      assert(alpha < beta);

      const auto is_check = move::is_check(mv, bd);

      bool dangerous = in_check || move::is_tactical(mv) || is_check;

      int ext = extension(sl, mv, depth, pv_node, is_check);
      int red = reduction(sl, mv, depth, pv_node, in_check, searched_size, dangerous); // LMR

      if (ext != 0) red = 0;
      assert(ext == 0 || red == 0);

      int sc;
      PV npv;

      move(sl, mv, is_check);

      if ((pv_node && searched_size != 0) || red != 0) {

        sc = -search<side::opposit(sd)>(sl, depth + ext - red - 1, -alpha - 1, -alpha, npv);

        if (sc > alpha) { // PVS/LMR re-search
          sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
        }

      } else {

        sc = -search<side::opposit(sd)>(sl, depth + ext - 1, -beta, -alpha, npv);
      }

      undo(sl);

      sp.update(mv, sc, npv);
    }
  }

  template<int sd>void master_split_point(SearchLocal & sl, SplitPoint & sp) {

    sp.enter();

    sl_push(sl, sp);

    try {
      search_split_point<sd>(sl, sp);
    } catch (const Abort & /* abort */) {
      // no-op
    }

    sl_pop(sl);

    sp.leave();

    idle_loop(sl, sp);
    sl.board = sp.board();

    assert(sp.free());

    // update move-ordering tables

    board::Board & bd = sl.board;

    int depth = sp.depth();
    int ply = bd.ply();

    int bs = sp.bs();
    int bm = sp.bm();

    assert(bs != score::NONE);

    if (bs >= sp.beta() && depth > 0 && !attack::is_in_check(bd) && !move::is_tactical(bm)) {
      sl.killer.add(bm, ply);
      g_sg.history.add(bm, sp.searched(), bd);
    }

    if (depth >= 0) {
      int flags = score::flags(bs, sp.old_alpha(), sp.beta());
      g_sg.trans.store(bd, depth, ply, bm, bs, flags);
    }
  }

  template<int sd>
    int qs_static(SearchLocal & sl, int beta, int gain) { // for static NMP

      board::Board & bd = sl.board;

      assert(attack::is_legal(bd));
      // assert(!attack::is_in_check()); // triggers for root move ordering

      // stand pat

      int bs = eval(sl);
      int val = bs + gain;

      if (bs >= beta) {
        return bs;
      }

      // move loop

      attack::Attacks attacks;
      attack::init_attacks(attacks, bd);

      gensort::List ml;
      ml.init(-1, bd, attacks, move::NONE, sl.killer, g_sg.history); // QS move generator

      bit::Bitboard done;
      done.init();

      for (int mv = ml.next<sd>(); mv != move::NONE; mv = ml.next<sd>()) {

        if (done.is_set(move::to(mv))) { // process only LVA capture for each opponent piece
          continue;
        }

        done.set(move::to(mv));

        int see = move::see(mv, 0, score::EVAL_MAX, bd); // TODO: beta - val?

        if (see <= 0) continue; // don't consider equal captures as "threats"

        int sc = val + see;

        if (sc > bs) {

          bs = sc;

          if (sc >= beta) {
            return sc;
          }
        }
      }

      assert(bs < beta);

      return bs;
    }

  void inc_node(SearchLocal & sl) {

    sl.node++;

    if (sl.node % NODE_PERIOD == 0) {

      bool abort = false;

      update_current();

      if (poll()) abort = true;

      if (g_time.node_limited && g_current.node >= g_time.node_limit) {
        abort = true;
      }

      if (g_time.time_limited && g_current.time >= g_time.time_limit) {
        abort = true;
      }

      if (g_time.smart && g_current.depth > 1 && g_current.time >= g_time.limit_0) {
        if (g_current.pos == 0 || g_current.time >= g_time.limit_1) {
          if (!(g_time.drop || g_current.fail_high) || g_current.time >= g_time.limit_2) {
            if (g_time.ponder) {
              g_time.flag = true;
            } else {
              abort = true;
            }
          }
        }
      }

      if (g_time.smart && g_current.depth > 1 && g_current.size == 1 && g_current.time >= g_time.limit_0 / 8) {
        if (g_time.ponder) {
          g_time.flag = true;
        } else {
          abort = true;
        }
      }

      if (abort) sg_abort();
    }

    if (sl_stop(sl)) {
      throw Abort();
    }
  }

  bool poll() {

    write_info_opt();

    g_sg.lock();

    if (!input::g_input.has_input()) {
      g_sg.unlock();
      return false;
    }

    std::string line;
    bool eof = !input::g_input.get_line(line);
    if (engine::g_engine.log) util::log(line);

    g_sg.unlock();

    if (false) {
    } else if (eof) {
      std::exit(EXIT_SUCCESS);
    } else if (line == "isready") {
      std::cout << "readyok" << std::endl;
      return false;
    } else if (line == "stop") {
      usi::g_infinite = false;
      return true;
    } else if (line == "ponderhit") {
      usi::g_infinite = false;
      g_time.ponder = false;
      return g_time.flag;
    } else if (line == "quit") {
      std::exit(EXIT_SUCCESS);
    }

    return false;
  }

  void sl_init_early(SearchLocal & sl, int id) {

    sl.id = id;

    sl.todo = true;
    sl.todo_sp = NULL;

    sl.node = 0;
    sl.max_ply = 0;

    sl.msp_stack_size = 0;
    sl.ssp_stack_size = 0;
  }

  void sl_init_late(SearchLocal & sl) {
    sl.killer.clear();
    //sl.pawn_table.clear(); // pawn-eval cache
    sl.eval_table.clear(); // eval cache
  }
  void sl_set_root(SearchLocal & sl, const board::Board & bd) {
    sl.board = bd;
    sl.board.set_root();
  }

  void sl_signal(SearchLocal & sl) {
    sl.lock();
    sl.signal();
    sl.unlock();
  }

  bool sl_stop(const SearchLocal & sl) {

    for (SplitPoint * sp = &sl_top(sl); sp != nullptr; sp = sp->parent()) {
      if (sp->solved()) return true;
    }

    return false;
  }

  bool sl_idle(const SearchLocal & worker, SplitPoint * sp) {

    assert(sp != nullptr);

    if (worker.todo) return false;
    if (worker.todo_sp != NULL) return false;

    SplitPoint & wait_sp = sl_top(worker);

    for (; sp != nullptr; sp = sp->parent()) {
      if (sp == &wait_sp) return true;
    }

    return false;
  }

  void sl_push(SearchLocal & sl, SplitPoint & sp) {
    assert(sl.ssp_stack_size < 16);
    sl.ssp_stack[sl.ssp_stack_size++] = &sp;
  }

  void sl_pop(SearchLocal & sl) {
    assert(sl.ssp_stack_size > 0);
    sl.ssp_stack_size--;
  }

  SplitPoint & sl_top(const SearchLocal & sl) {
    assert(sl.ssp_stack_size > 0);
    return *sl.ssp_stack[sl.ssp_stack_size - 1];
  }

  void sg_abort() {

    g_root_sp.update_root();

    for (int id = 0; id < engine::g_engine.threads; id++) {
      sl_signal(g_sl[id]);
    }
  }

  void move(SearchLocal & sl, const int mv, const bool check) {

    board::Board & bd = sl.board;

    inc_node(sl);
    bd.move(mv,check);

    int ply = bd.ply();

    if (ply > sl.max_ply) {
      assert(ply <= MAX_PLY);
      sl.max_ply = ply;
    }
  }

  void undo(SearchLocal & sl) {
    board::Board & bd = sl.board;
    bd.undo();
  }

  int eval(SearchLocal & sl) {
    board::Board & bd = sl.board;
    return eval::eval(bd, sl.eval_table);
  }

  int extension(SearchLocal & /*sl*/, int /*mv*/, int depth, bool pv_node, bool check) {

    //board::Board & bd = sl.board;

    if ((depth <= 4 && check)
        || (pv_node && check)
        /* || (pv_node && move::is_tactical(mv) && move::is_win(mv, bd))*/) {
      return 1;
    } else {
      return 0;
    }
  }

  int reduction(SearchLocal & /* sl */, int /* mv */, int depth, bool /* pv_node */, bool /* in_check */, int searched_size, bool dangerous) {

    int red = 0;

    if (depth >= 3 && searched_size >= 3 && !dangerous) {
      red = (searched_size >= 6) ? depth / 3 : 1;
    }

    return red;
  }

  int limit_extension(const int ext, const int ply) {
    if(ext && ply > 2 * g_current.depth) {//HACK
      return 0;
    } else {
      return ext;
    }
  }

  template<int sd>void gen_sort(SearchLocal & sl, gen::List & ml) {

    board::Board & bd = sl.board;

    gen::add_legal_move<sd>(ml,bd);

    //int v = eval(sl);

    for (auto pos = 0; pos < ml.size(); ++pos) {

      auto mv = ml.move(pos);

      move(sl, mv, move::is_check(mv,bd));
      auto sc = -qs_static<side::opposit(sd)>(sl, score::MAX, 0);
      undo(sl);

      //sc = ((sc - v) / 4) + 1024; // HACK for unsigned 11-bit move-list scores
      sc += score::MAX;
      assert(sc >= 0 && sc < move::SCORE_SIZE);

      ml.set_score(pos, sc);
    }

    ml.sort();
  }
  void gen_sort(SearchLocal & sl, gen::List & ml) {
    board::Board & bd = sl.board;
    (bd.turn() == side::BLACK) ? gen_sort<side::BLACK>(sl,ml)
      : gen_sort<side::WHITE>(sl,ml);
  }
  template<int sd>void search_asp(gen::List & ml, int depth) {

    SearchLocal & sl = g_sl[0];

    assert(depth <= 1 || g_time.last_score == g_best.score);

    if (depth >= 6 && !score::is_mate(g_time.last_score)) {

      for (int margin = 10; margin < 500; margin *= 2) {

        int a = g_time.last_score - margin;
        int b = g_time.last_score + margin;
        assert(score::EVAL_MIN <= a && a < b && b <= score::EVAL_MAX);

        search_root<sd>(sl, ml, depth, a, b);

        if (g_best.score > a && g_best.score < b) {
          return;
        } else if (score::is_mate(g_best.score)) {
          break;
        }
      }
    }

    search_root<sd>(sl, ml, depth, score::MIN, score::MAX);
  }

  template<int sd>void search_id(const board::Board & bd) {

    SearchLocal & sl = g_sl[0];

    sl_set_root(sl, bd);

    sl_push(sl, g_root_sp);

    // move generation

    gen::List ml;
    gen_sort(sl, ml);

    if(ml.size() == 0) {
      g_best.move = move::NONE;//念の為
      return;
    }

    assert(ml.size() != 0);

    g_best.move = ml.move(0);
    g_best.score = 0;

    bool easy = (ml.size() == 1 || (ml.size() > 1 && ml.score(0) - ml.score(1) >= 50 / 4)); // HACK: uses gen_sort() internals
    int easy_move = ml.move(0);

    g_time.last_score = score::NONE;

    // iterative deepening

    assert(g_time.depth_limited);

    for (int depth = 1; depth <= g_time.depth_limit; depth++) {

      depth_start(depth);
      search_asp<sd>(ml, depth);
      depth_end();

      // p_time.drop = (best.score <= p_time.last_score - 50); // moved to update_best()
      g_time.last_score = g_best.score;

      if (g_best.move != easy_move || g_time.drop) {
        easy = false;
      }

      if (g_time.smart && !g_time.drop) {

        bool abort = false;

        update_current();

        if (ml.size() == 1 && g_current.time >= g_time.limit_0 / 16) {
          abort = true;
        }

        if (easy && g_current.time >= g_time.limit_0 / 4) {
          abort = true;
        }

        if (g_current.time >= g_time.limit_0 / 2) {
          abort = true;
        }

        if (abort) {
          if (g_time.ponder) {
            g_time.flag = true;
          } else {
            break;
          }
        }
      }
    }

    sl_pop(sl);
  }
  void search_id(const board::Board & bd) {
    (bd.turn() == side::BLACK) ? search_id<side::BLACK>(bd)
      : search_id<side::WHITE>(bd);
  }
  void search_go(const board::Board & bd) {

    clear();

    init_sg();
    g_sg.trans.inc_date();

    for (int id = 0; id < engine::g_engine.threads; id++) {
      sl_init_early(g_sl[id], id);
    }

    g_root_sp.init_root(g_sl[0]);

    for (int id = 1; id < engine::g_engine.threads; id++) { // skip 0
      g_sl[id].thread = std::thread(helper_program, &g_sl[id]);
    }

    sl_init_late(g_sl[0]);

    try {
      search_id(bd);
    } catch (const Abort & /* abort */) {
      // no-op
    }

    sg_abort();

    for (int id = 1; id < engine::g_engine.threads; id++) { // skip 0
      g_sl[id].thread.join();
    }

    search_end();
  }
  void search_dumb(const board::Board & bd) {

    g_time.smart = false;
    g_time.last_score = score::NONE;
    g_time.drop = false;

    search_go(bd);
  }

  void search_smart(const board::Board & bd, int moves, int64 time, int64 inc, int64 byoyomi) {

    if (moves == 0)
      moves = 40;
    //moves = std::min(moves, material::interpolation(35, 15, bd));
    assert(moves > 0);

    int64 total = time + inc * (moves - 1);
    int factor = engine::g_engine.ponder ? 140 : 120;
    int64 alloc = total / moves * factor / 100;
    int64 reserve = total * (moves - 1) / 40;
    int64 max = std::min(time, total - reserve);
    max = std::min(max - 60, max * 95 / 100); // 60ms for lag

    alloc = std::max(alloc, I64(0));
    max = std::max(max, I64(0));
    auto limit_0 = std::min(alloc, max);
    auto limit_1 = std::min(alloc * 4, max);
    auto limit_2 = max;
    if(total < 1000 && byoyomi) {
      limit_0 = byoyomi - 800;
      limit_1 = byoyomi - 800;
      limit_2 = byoyomi - 800;
    }

    g_time.smart = true;
    g_time.limit_0 = limit_0;
    g_time.limit_1 = limit_1;
    g_time.limit_2 = limit_2;
    g_time.last_score = score::NONE;
    g_time.drop = false;

    assert(
        0 <= g_time.limit_0 && g_time.limit_0 <= g_time.limit_1
        && g_time.limit_1 <= g_time.limit_2);

    search_go(bd);
  }
  template<int sd> uint64 perft(board::Board & bd, int ply) {
    assert(bd.is_ok());
    if (ply == 0) {
      return 1;
    }
    attack::Attacks att;
    attack::init_attacks(att, bd);
    gen::List ml;
    if (!att.size) {
      gen::add_move<gen::TACTICAL, sd>(ml, bd);
      gen::add_move<gen::QUIET, sd>(ml, bd);
      gen::add_move<gen::DROP, sd>(ml, bd);
    } else {
      gen::add_move<gen::EVASION, sd>(ml, bd, &att);
    }

    auto num = 0;
    for (auto pos = 0; pos < ml.size(); pos++) {
      const auto mv = ml.move(pos);
      if (!move::is_legal(mv, bd, att)) {
        continue;
      }
      bd.move(mv,move::is_check(mv,bd));
      num += perft<side::opposit(sd)>(bd, ply - 1);
      bd.undo();
    }
    return num;
  }
  uint64 perft(board::Board & bd, int ply) {
    return (bd.turn() == side::BLACK) ?
      perft<side::BLACK>(bd, ply) : perft<side::WHITE>(bd, ply);
  }
  void init() {
    g_sg.trans.set_size(engine::g_engine.hash);
    g_sg.trans.alloc();
  }
}

namespace test {
  void test_square() {
    for (auto file = 0; file < square::FILE_SIZE; file++) {
      for (auto rank = 0; rank < square::RANK_SIZE; rank++) {
        auto sq = square::make(file, rank);
        util::Tee << "sq:" << sq << " file:" << file << " rank:" << rank
          << "\n";
        auto str = square::to_string(sq);
        util::Tee << "usi:" << str << "\n";
        util::Tee << "conv:" << square::from_string(str) << "\n";
        util::Tee << bit::g_mask[sq] << std::endl;
      }
    }
  }
  void test_piece() {
    for (auto p = 0; p < piece::SIZE; p++) {
      auto str = piece::to_char(p);
      auto p2 = piece::from_string(str);
      util::Tee << p << ":" << str << ":" << p2 << std::endl;
      assert(p == p2);
    }
    for (auto p32 = 0; p32 < piece::SIDE_SIZE; p32++) {
      auto str = piece::to_sfen(p32);
      auto p2 = piece::from_sfen(str);
      util::Tee << p32 << ":" << str << ":" << p2 << std::endl;
      assert(p32 == p2);
    }
  }
  void test_bit() {
    using namespace util;
    for (auto sq = 0; sq < square::SIZE; sq++) {
      Tee << "sq:" << sq << std::endl;
      Tee << bit::g_rook_mask[sq] << std::endl;
      Tee << bit::g_bishop_mask[sq] << std::endl;
    }
    for (auto sq = 0u; sq < attack::g_pawn_attacks[0].size(); sq++) {
      Tee << "sq:" << sq << std::endl;
      Tee << attack::g_pawn_attacks[0][sq] << std::endl;
      Tee << attack::g_pawn_attacks[1][sq] << std::endl;
    }
  }
  void test_attack() {

    //bit::Bitboard occ = bit::g_bishop_mask[square::SQ_55];
    // while(!occ.is_empty()) {
    //   util::Tee<<"occ:"<<std::endl;
    //   util::Tee<<occ<<std::endl;
    //   util::Tee<<"att:"<<std::endl;
    //   util::Tee<<attack::get_bishop_attack(square::SQ_55,occ)<<std::endl;
    //
    //   occ.lsb();
    // }
    //
    // occ = bit::g_rook_mask[square::SQ_55];
    // while(!occ.is_empty()) {
    //   util::Tee<<"occ:"<<std::endl;
    //   util::Tee<<occ<<std::endl;
    //   util::Tee<<"att:"<<std::endl;
    //   util::Tee<<attack::get_rook_attack(square::SQ_55,occ)<<std::endl;
    //
    //   occ.lsb();
    // }
    // occ = attack::g_lance_attacks[side::BLACK][square::SQ_55][0];
    // while(!occ.is_empty()) {
    //   util::Tee<<"occ:"<<std::endl;
    //   util::Tee<<occ<<std::endl;
    //   util::Tee<<"att:"<<std::endl;
    //   util::Tee<<attack::get_lance_attack(side::BLACK,square::SQ_55,occ)<<std::endl;
    //
    //   occ.lsb();
    // }
    //	occ = attack::g_lance_attacks[side::WHITE][square::SQ_55][0];
    //	while (!occ.is_empty()) {
    //		util::Tee << "occ:" << std::endl;
    //		util::Tee << occ << std::endl;
    //		util::Tee << "att:" << std::endl;
    //		util::Tee << attack::get_lance_attack(side::WHITE, square::SQ_55, occ)
    //				<< std::endl;
    //
    //		occ.lsb();
    //	}
    {
      board::Board bd("ln1+N1+R3/2G6/1pp5p/p5p2/1NG1sp3/PkP6/3+r2P1P/L1S6/K3+b2NL b  - 111");
      std::cout<<bd<<std::endl;
      const auto to = bd.king(side::WHITE) + square::INC_DOWN;
      const auto b = attack::is_mate_with_pawn_drop(to,bd);
      assert(!b);
    }
    {
      board::Board bd("l7l/2P6/3Spk3/3P2p1p/pp2P1sK1/2S2P2P/PP2R2P1/9/LN2G2+bL w  - 1");
      std::cout<<bd<<std::endl;
      const auto to = bd.king(side::BLACK) + square::INC_UP;
      const auto b = attack::is_mate_with_pawn_drop(to,bd);
      assert(b);
    }
  }
  void test_board() {
    board::Board bd;
    bd.init_sfen(board::start_sfen);
    util::Tee << bd << std::endl;

    gen::List ml;
    ml.clear();
    gen::add_move<gen::QUIET, side::BLACK>(ml, bd);
    assert(ml.size() == 30);
    for (auto m : ml) {
      const auto mv = gen::List::move(m);
      util::Tee << move::to_can(mv) << std::endl;
      bd.move(mv,move::is_check(mv,bd));

      //util::Tee<<bd<<std::endl;

      bd.undo();

      //util::Tee<<bd<<std::endl;
    }
  }
  void test_hand() {
    uint32 hand = 0;
    int max[] = { 20, 4, 4, 4, 2, 2, 4 };
    for (auto pc = int(piece::PAWN); pc <= piece::GOLD; pc++) {
      for (auto i = 0; i < max[hand::to_hp(pc)]; i++) {
        hand = hand::change<true>(hand, pc);
        auto num = hand::num(hand, pc);
        assert(num == i + 1);
      }
      for (auto i = max[hand::to_hp(pc)]; i > 0; i--) {
        hand = hand::change<false>(hand, pc);
        auto num = hand::num(hand, pc);
        assert(num == i - 1);

      }
    }
  }
  void test_gen() {
    board::Board bd(board::start_sfen);
    gen::List ml;
    gen::add_legal_move(ml,bd);
    //util::Tee<<bd<<std::endl;
    for(auto i = 0; i < ml.size(); i++) {
      //const auto mv = ml.move(i);
      //util::Tee << i << ":" << move::to_can(mv) << std::endl;
    }
    assert(ml.size() == 30);
    /*bd.init_sfen("4k4/9/9/9/9/9/PPP1PPPPP/9/4K4 b P - 1");
      util::Tee << bd << std::endl;
      gen::List ml;
      gen::add_move<gen::DROP, side::BLACK>(ml, bd);
      for (auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      util::Tee << i << ":" << move::to_can(mv) << std::endl;
      }*/

    bd.init_sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1");
    ml.clear();
    gen::add_legal_move(ml,bd);
    for (auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      util::Tee << i << ":" << move::to_can(mv) << std::endl;
    }
    assert(ml.size() == 30);

    bd.init_sfen("kn5n1/lsgg3r1/ppppp1b1p/4sp3/PPP3pp1/1R7/3PPPPPP/1B3GGSL/LNS4NK b - 38");
    ml.clear();
    gen::add_legal_move(ml,bd);
    //util::Tee<<bd<<std::endl;
    for (auto i = 0; i < ml.size(); i++) {
      //const auto mv = ml.move(i);
      //util::Tee << i << ":" << move::to_can2(mv) << std::endl;
    }
    assert(ml.size() == 34);

    bd.init_sfen("l6nl/9/6s2/p2kppppp/2pN5/P1NPPgP1P/2S4P1/6SK1/+r3BG1NL w  - 111");
    ml.clear();
    gen::add_legal_move(ml,bd);
    util::Tee<<bd<<std::endl;
    for (auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      util::Tee << i << ":" << move::to_can2(mv) << std::endl;
    }
    assert(ml.size() == 3);

    bd.init_sfen("lnsgkgsnl/1r5b1/pppppp1p1/6p1p/9/2P5P/PPBPPPPP1/7R1/LNSGKGSNL w - 6");
    ml.clear();
    gen::add_legal_move(ml,bd);
    util::Tee<<bd<<std::endl;
    for (auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      util::Tee << i << ":" << move::to_can2(mv) << std::endl;
    }
    assert(ml.size() == 3);
  }
  void test_playout() {

    for (;;) {
      board::Board bd;
      bd.init_sfen(board::start_sfen);
      while (true) {
        std::cout << bd << std::endl;
        gen::List ml;
        gen::add_legal_move(ml, bd);
        if (ml.size() == 0) {
          break;
        }
        if (bd.sp() > 500) {
          break;
        }
        search::new_search();
        search::set_time_limit(2000);
        search::search_dumb(bd);
        const auto mv = search::g_best.move;
        bd.move(mv,move::is_check(mv,bd));
        std::cout <<" val: "<< search::g_best.score <<":"<< move::to_can(mv) << std::endl;
      }
    }
  }
  void test_record() {
    board::Board bd;
    std::ifstream fin;
    fin.open("records.sfen");
    auto i = 0u;
    while(!fin.eof()) {
      util::Tee<<i++<<std::endl;;
      std::string buf;
      std::getline(fin,buf);
      const auto index = buf.find("moves");
      if(index == std::string::npos) {
        break;
      }
      std::string pos   = buf.substr(5,index);
      std::string moves = buf.substr(index + 6);

      if(buf.find("startpos") != std::string::npos) {
        bd.init_sfen(board::start_sfen);
      } else {
        bd.init_sfen(pos);
      }

      std::vector<std::string> str_list;
      util::split(moves," ",str_list);
      for(auto move_str : str_list) {
        //util::Tee<<bd<<std::endl;
        //util::Tee<<move_str<<std::endl;
        const auto mv = move::from_string(move_str,bd);
        if(!gen::is_move(mv,bd)){
          util::Tee<<"record error\n";
          util::Tee<<bd<<std::endl;
          util::Tee<<move_str<<std::endl;
          break;
        }
        if(!move::is_legal_debug(mv,bd)) {
          util::Tee<<"illgal move error\n";
          util::Tee<<bd<<std::endl;
          util::Tee<<move_str<<std::endl;
          break;
        }
        gen::List ml;
        gen::add_legal_move(ml,bd);
        if(!ml.contain_bona(mv)) {
          util::Tee<<"not found\n";
          util::Tee<<bd<<std::endl;
          util::Tee<<move_str<<std::endl;
          util::Tee<<move::to_can(mv)<<std::endl;
          for(auto i = 0; i < ml.size(); i++) {
            util::Tee<<move::to_can(ml.move(i))<<std::endl;
          }
          assert(false);
        }
        util::Tee<<"nodes:"<<search::perft(bd,2)<<std::endl;
        bd.move(mv,move::is_check(mv,bd));
      }
    }
  }
  void test_see() {
    board::Board bd("4k4/2s3g2/plnsgbr+p+l/2G2GSLP/PP2P+r+b+s+n/9/5N3/9/4K4 b Pnl11p - 1");
    gen::List ml;
    util::Tee<<bd<<std::endl;
    //gen::add_legal_move(ml,bd);
    gen::add_move<gen::TACTICAL,side::BLACK>(ml,bd);
    for(auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::see(mv,score::MIN,score::MAX,bd)<<std::endl;
    }
  }
  void test_check() {

    {
      board::Board bd("ln1gs4/4gksbl/pr1p1pn1p/2p3pp1/P3B4/5P3/2PPGGPPP/3S1K2R/LN4SNL w 2P2p - 50");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        const auto mv = ml.move(i);
        if(i != 32) continue;
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    return;
    {
      board::Board bd("2gs2s2/r4gkb1/1p1N1pppn/2ppp4/1P4n2/P1P3+r1p/3PP1N2/LBGGS1K2/2S5L w P2L4p - 75");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        if(i != 15) continue;
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    {
      board::Board bd("l23sn1/+Rsk1G1g2/p2p1p1pl/1p4P1p/2p6/9/PP2KP1PP/2G3R2/+bN2SG1NL b 2PN3plsb - 56");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        if(i != 22) {
          continue;
        }
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    {
      board::Board bd("ln5nl/r2s1k1+P1/1pppppb2/p5pB1/6g1p/P1P6/1PNPPPP2/S2G2G+l1/2G2KSN1 b 2PSlr - 58");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    {
      board::Board bd("4k4/9/9/9/9/9/4N4/4L4/4K4 b 2r2b4g4s3n3l18p - 1");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      //gen::add_move<gen::TACTICAL,side::BLACK>(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    {
      board::Board bd("4k4/R8/9/9/4Nl1b1/4RPl2/5K3/4g4/4bss2 b 3g2s3n2l17p - 1");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      //gen::add_move<gen::TACTICAL,side::BLACK>(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
    {
      board::Board bd("4k4/R8/9/9/4Nl1b1/4RPl2/5K3/4g4/4bss2 w 3g2s3n2l17p - 1");
      gen::List ml;
      util::Tee<<bd<<std::endl;
      gen::add_legal_move(ml,bd);
      //gen::add_move<gen::TACTICAL,side::BLACK>(ml,bd);
      for(auto i = 0; i < ml.size(); i++) {
        const auto mv = ml.move(i);
        util::Tee<<i+1<<":"<<move::to_can2(mv)<<":"<<move::is_check(mv,bd)<<std::endl;
      }
    }
  }
  void test_search() {
    board::Board bd("lnsgk1snl/1r4gb1/pp1ppp1pp/6pP1/2p6/2P6/PP1PPPP1P/1B5R1/LNSGKGSNL b - 1");
    search::new_search();
    search::set_time_limit(12000);
    search::search_dumb(bd);
    const auto mv = search::g_best.move;
    std::cout <<" val: "<< search::g_best.score <<":"<< move::to_can(mv) << std::endl;
  }
  void test_mate() {
    board::Board bd;
    std::ifstream fin;
    fin.open("mate3");
    auto i = 0u;
    while(!fin.eof()) {
      util::Tee<<i++<<std::endl;;
      std::string buf;
      std::getline(fin,buf);
      const auto index = buf.find("moves");
      if(index == std::string::npos) {
        break;
      }
      std::string pos   = buf.substr(5,index);
      std::string moves = buf.substr(index + 6);

      if(buf.find("startpos") != std::string::npos) {
        bd.init_sfen(board::start_sfen);
      } else {
        bd.init_sfen(pos);
      }

      std::vector<std::string> str_list;
      util::split(moves," ",str_list);
      util::Tee<<bd<<std::endl;
      util::Tee<<str_list[0]<<std::endl;
      const auto mv = move::from_string(str_list[0],bd);
      search::new_search();
      search::set_depth_limit(4);
      search::search_dumb(bd);
      const auto mv2 = search::g_best.move;
      util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;

      if(mv != mv2) {
        util::Tee<<"not_eq\n";
      }
    }
  }
  void test_mate2() {
    //	{
    //	board::Board bd("1n5+Rl/k3+P4/1p+b+P5/P1g5p/lP1n5/1K2PP2P/B2sS1P2/9/L+n7 w PN2S2GR7plg 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    //	{
    //	board::Board bd("1n5+Rl/k3+P4/1p+b+P5/P1g5p/lP1n5/1l2PP2P/B2sS1P2/K8/L+n7 w PN2S2GR7pg 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    //	{
    //	board::Board bd("1n5+Rl/k3+P4/1p+b+P5/P1g5p/lP1n5/1B2PP2P/1K1sS1P2/9/L+n w PN2S2GR7plg 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    //	{
    //	board::Board bd("1n5+Rl/k3+P4/1p+b+P5/P1g5p/lP1n5/4PP2P/BK1sS1P2/9/L+n w PN2S2GR7plg 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    {
      board::Board bd("l2g4l/1ks6/Ppn2p3/2K4pp/1P1N1PpP1/l2s1G3/1G2+r1P1P/2+n6/s6NL w S2BRG7p - 1");
      //board::Board bd("l2g4l/1ks6/Ppn2p3/2K1+r2pp/1P1N1PpP1/l2s1G3/1G4P1P/2+n6/s6NL b S2BRG7p - 1");
      util::Tee<<bd<<std::endl;
      search::new_search();
      search::set_depth_limit(3);
      search::search_dumb(bd);
      const auto mv2 = search::g_best.move;
      util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    }
    //	{
    //	//board::Board bd("l5p1l/1kg2+R1p1/1l1+Np3p/2K6/2Pp5/9/3P1P2P/3G+r4/8L w 9P2N3S2B2Gns - 1");
    //	board::Board bd("l5p1l/1kg2+R1p1/1l1+Np3p/2K1+r4/2Pp5/9/3P1P2P/3G5/8L b 9P2N3S2B2Gns - 1");
    //
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    //	{
    //	board::Board bd("l5p1l/1kg+R3p1/1l1+Np3p/2K6/2Pp5/9/3P1P2P/3G+r4/8L w 9P3N3S2B2Gs - 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
    //	{
    //	board::Board bd("kn+BR4l/lg2P4/1sp6/1p2+r2pP/s1K2pP2/p8/1bN2P3/L1s6/8L w P2NS3G8p - 1");
    //	//board::Board bd("kn+BR4l/lg2P4/1sp6/1p5pP/s1K1+rpP2/p8/1bN2P3/L1s6/8L b P2NS3G8p - 1");
    //	util::Tee<<bd<<std::endl;
    //	search::new_search();
    //	search::set_depth_limit(5);
    //	search::search_dumb(bd);
    //	const auto mv2 = search::g_best.move;
    //	util::Tee <<" val: "<< search::g_best.score <<":"<< move::to_can(mv2) << std::endl;
    //	}
  }
  void test_trans() {

    {
      search::g_sg.trans.clear();
      board::Board bd("lnsgkgsnl/1r5b1/ppppppp1p/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b P");
      search::g_sg.trans.store(bd,1,1,move::NONE,2,score::FLAGS_LOWER);
      int move;
      int score;
      int flags;
      assert(search::g_sg.trans.retrieve(bd,1,1,move,score,flags));
      board::Board bd2("lnsgkgsnl/1r5b1/ppppppp1p/9/9/9/PPPPPPPP1/1B5R1/LNSGKGSNL b 2P");
      assert(!search::g_sg.trans.retrieve(bd2,1,1,move,score,flags));
    }
  }
  void test_mate1ply() {
    {
      board::Board bd("2r1k1bnl/1s3s+L2/l2ppgp1p/1g7/n1p2p3/1p2P1P2/K1PP1G1PP/1BS1G3R/1N4SNL w 3P2p - 64");
      std::cout<<bd<<std::endl;
      attack::Attacks attacks;
      attack::init_attacks(attacks,bd);
      assert(move::NONE != mate1::mate_in_1ply<side::WHITE>(bd,attacks.pinned));
    }
    {
      board::Board bd("3+R2s1k/8l/1p2B2+B1/1g2pppp1/3N1n2p/1Pp1P1P2/3PgP1PP/6S1R/1+l2GK1NL b PSG3plns - 101");
      std::cout<<bd<<std::endl;
      attack::Attacks attacks;
      attack::init_attacks(attacks,bd);
      assert(move::NONE != mate1::mate_in_1ply<side::BLACK>(bd,attacks.pinned));
    }
    {
      board::Board bd("lns2g1nl/1r2gs1b1/1pp1ppppp/p2p5/k8/3B5/PPNPPPPPP/6SRL/L1SGKG1N1 b p - 21");
      attack::Attacks attacks;
      attack::init_attacks(attacks,bd);
      assert(move::NONE != mate1::mate_in_1ply<side::BLACK>(bd,attacks.pinned));
    }
    {
      board::Board bd(board::start_sfen);
      attack::Attacks attacks;
      attack::init_attacks(attacks,bd);
      assert(move::NONE == mate1::mate_in_1ply<side::BLACK>(bd,attacks.pinned));
    }
    {
      board::Board bd("1nsk1gsn+B/lr3g3/1ppp3p1/p3p1p1p/9/2P1P1P2/PP1P1P1PP/L3K1S1R/1N+b1GG1NL w PLs - 26");
      attack::Attacks attacks;
      attack::init_attacks(attacks,bd);
      assert(move::NONE != mate1::mate_in_1ply<side::WHITE>(bd,attacks.pinned));
    }

  }
  void test_eval() {
    board::Board bd(board::start_sfen);
    gen::List ml;
    gen::add_legal_move(ml,bd);
    for(auto i = 0; i < ml.size(); i++) {
      const auto mv = ml.move(i);
      bd.move(mv,false);
      std::cout<<move::to_can2(mv)<<":"<<eval::comp_eval(bd)<<std::endl;
      bd.undo();
    }
  }
  void test_test() {
    board::Board bd("l+P1g1gsnl/6kR1/p2ppp1pp/6p2/9/3P5/P3PPPPP/5K3/LN+p2GSNL w NSB2psbrg");
    gen::List ml;
    bit::Bitboard bb;
    bb = bd.all() ^ bit::g_all_one;
    int piece_list[4] = { piece::SILVER, piece::GOLD, piece::BISHOP, piece::ROOK };
    gen::add_drop_move<false,false,true,side::WHITE,4>(ml,bd,bb,piece_list);
    std::cout<<bd<<std::endl;
  }
  int test() {
    util::Tee<<"start test\n";
    //test_square();
    //test_piece();
    //test_bit();
    //test_attack();
    //test_board();
    //test_gen();
    //test_hand();
    //test_playout();
    //test_record();
    //test_attack();
    //test_see();
    //test_check();
    //test_search();
    //test_mate();
    //test_mate2();
    //test_trans();
    //test_mate1ply();
    //test_eval();
    test_test();
    util::Tee<<"end test\n";
    return 0;
  }
}

namespace usi {

  board::Board g_bd;
  bool g_delay;

  class Scanner {

    private:

      std::stringstream * ss_;
      std::vector<std::string> keywords_;

      bool undo_;
      std::string word_;

      bool is_keyword(const std::string & word) const {

        for (int i = 0; i < int(keywords_.size()); i++) {
          if (keywords_[i] == word)
            return true;
        }

        return false;
      }

    public:

      Scanner(std::stringstream & ss) {
        ss_ = &ss;
        undo_ = false;
        add_keyword("");
      }

      void add_keyword(const std::string & keyword) {
        keywords_.push_back(keyword);
      }

      std::string get_keyword() {

        std::string word = get_word();
        assert(is_keyword(word));

        return word;
      }

      std::string get_args() {

        std::string args;
        std::string word;

        while (true) {

          std::string word = get_word();

          if (is_keyword(word)) {
            unget_word();
            break;
          }

          if (args != "")
            args += " ";
          args += word;
        }

        return args;
      }

      std::string get_word() {

        if (undo_) {
          undo_ = false;
        } else if (!(*ss_ >> word_)) { // NOTE: reads as a side effect
          word_ = "";
        }

        return word_;
      }

      void unget_word() {
        assert(!undo_);
        undo_ = true;
      }

  };

  void sfen(const std::string & sfen) {
    g_bd.init_sfen(sfen);
  }
  void move(const std::string & move) {
    const auto mv = move::from_string(move, g_bd);
    g_bd.move(mv,move::is_check(mv,g_bd));
  }
  void send_bestmove() {
    if(search::g_best.move != move::NONE) {
      util::Tee << "bestmove " << move::to_can(search::g_best.move) << std::endl;
    } else {
      util::Tee << "bestmove resign" << std::endl;
    }
    g_delay = false;
  }
  void command(Scanner & scan) {
    auto command = scan.get_word();
    if (false) {
    } else if (command == "usi") {
#ifdef DEBUG
      util::Tee << "id name SHOGI_DEBUG" << std::endl;
#else
      util::Tee<<"id name SHOGI"<<std::endl;
#endif
      util::Tee << "usiok" << std::endl;
    } else if (command == "isready") {
      util::Tee << "readyok" << std::endl;
    } else if (command == "setoption") {
      //TODO
    } else if (command == "usinewgame") {
      search::g_sg.trans.clear();
    } else if (command == "position") {
      scan.add_keyword("sfen");
      scan.add_keyword("startpos");
      scan.add_keyword("moves");

      std::string part;
      while ((part = scan.get_keyword()) != "") {
        if (false) {
        } else if (part == "sfen") {
          sfen(scan.get_args());
        } else if (part == "startpos") {
          sfen(board::start_sfen);
        } else if (part == "moves") {
          std::string arg;
          while ((arg = scan.get_word()) != "") {
            usi::move(arg);
          }
        }
      }
    } else if (command == "go") {
      scan.add_keyword("searchmoves");
      scan.add_keyword("ponder");
      scan.add_keyword("wtime");
      scan.add_keyword("btime");
      scan.add_keyword("winc");
      scan.add_keyword("binc");
      scan.add_keyword("movestogo");
      scan.add_keyword("depth");
      scan.add_keyword("nodes");
      scan.add_keyword("mate");
      scan.add_keyword("movetime");
      scan.add_keyword("infinite");
      scan.add_keyword("byoyomi");

      search::new_search();

      g_infinite = false;
      g_delay = false;

      auto smart = false;
      auto time = 0;
      auto inc = 0;
      auto byoyomi = 0;
      int movestogo = 0;

      std::string part;
      while ((part = scan.get_keyword()) != "") {
        std::string args = scan.get_args();
        if (false) {
        } else if (part == "ponder") {
          g_infinite = false;
          search::set_ponder();
        } else if (part == "wtime") {
          if (g_bd.turn() == side::WHITE) {
            smart = true;
            time = int(util::to_int(args));
          }
        } else if (part == "btime") {
          if (g_bd.turn() == side::BLACK) {
            smart = true;
            time = int(util::to_int(args));
          }
        } else if (part == "winc") {
          if (g_bd.turn() == side::WHITE) {
            smart = true;
            inc = int(util::to_int(args));
          }
        } else if (part == "binc") {
          if (g_bd.turn() == side::BLACK) {
            smart = true;
            inc = int(util::to_int(args));
          }
        } else if (part == "movestogo") {
          smart = true;
          movestogo = int(util::to_int(args));
        } else if (part == "depth") {
          search::set_depth_limit(int(util::to_int(args)));
        } else if (part == "nodes") {
          search::set_node_limit(util::to_int(args));
        } else if (part == "movetime") {
          search::set_time_limit(int(util::to_int(args)));
        } else if (part == "infinite") {
          g_infinite = true;
        } else if(part == "byoyomi") {
          smart = true;
          byoyomi = int(util::to_int(args));
        }
      }
      if (smart) {
        search::search_smart(g_bd, movestogo, time, inc, byoyomi);
      } else {
        search::search_dumb(g_bd);
      }
      if (g_infinite) { // let's implement the USI-design mistake :(
        g_delay = true;
      } else {
        send_bestmove();
      }
    } else if (command == "test") {
      test::test();
    } else if (command == "show") {
      util::Tee << g_bd << std::endl;
    } else if (command == "stop") {
      if (g_delay) {
        send_bestmove();
      }
    } else if (command == "ponderhit") {
      if (g_delay) {
        send_bestmove();
      }
    } else if (command == "quit") {
      std::exit(EXIT_SUCCESS);
    }
  }
  void line(const std::string & line) {
    if (engine::g_engine.log) {
      util::log(line);
    }
    std::stringstream args(line);
    Scanner scan(args);
    command(scan);
  }
  void loop() {
    util::Tee << std::boolalpha;
    g_infinite = false;
    g_delay = false;
    sfen(board::start_sfen);
    std::string line;
    while (input::g_input.get_line(line)) {
      usi::line(line);
    }
  }
}

int main(int /*argc*/, char * /*argv*/[]) {

  assert(sizeof(uint8) == 1);
  assert(sizeof(uint16) == 2);
  assert(sizeof(uint32) == 4);
  assert(sizeof(uint64) == 8);

  util::init();
  input::init();
  bit::init();
  hash::init();
  castling::init();
  attack::init();
  engine::init();
  material::init();
  pst::init();
  pawn::init();
  eval::init();
  search::init();
  
  usi::loop();

  return EXIT_SUCCESS;
}
