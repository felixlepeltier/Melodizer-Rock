#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/int/arithmetic.hh>
#include <gecode/set.hh>
#include <vector>

using namespace Gecode;

namespace {
  int majorNatural[7] = {2, 2, 1, 2, 2, 2, 1};
  int minorNatural[7] = {2, 1, 2, 2, 2, 1, 2};
};

class Melody : public Script {
private:

  const int bars = 4;
  const int quantification = 8;

  SetVarArray push;
  SetVarArray pull;
  SetVarArray playing;

public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127),
    pull(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127),
    playing(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127){

      rel(*this, pull[0] == IntSet::empty);
      rel(*this, playing[0] == push[0]);

      for(int i = 1; i < bars*quantification; i++){
        // Notes that are playing
        rel(*this, playing[i] == ((playing[i-1] - pull[i]) | push[i])); 
        // Cannot pull a note not playing
        rel(*this, pull[i] <= playing[i-1]);
        // Cannot push a note still playing
        rel(*this, push[i] || (playing[i-1] - pull[i])); 
      }

      cardinality(*this, playing, 0, 10);
      cardinality(*this, pull, 0, 10);
      cardinality(*this, push, 0, 5);
      

      // Following a scale

      std::vector<int> v;
      for (int octave = 0; octave < 11; octave++){
        for (int i = 0; i<7; i++){
          v.push_back(octave*12+majorNatural[i]);
        }
      }

      IntArgs a(v);
      IntSet scaleSet(a);

      for(int i = 1; i < bars*quantification; i++){
        rel(*this, push[i] <= scaleSet);
      }
      

      Rnd r1(opt.seed());
      r1.time();
      Rnd r2(opt.seed());
      r2.time();

      branch(*this, push, SET_VAR_RND(r1), SET_VAL_RND_INC(r2));
      branch(*this, pull, SET_VAR_RND(r1), SET_VAL_RND_INC(r2));

  }

  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody){

      push.update(*this, melody.push);
      pull.update(*this, melody.pull);
      playing.update(*this, melody.playing);

  }

  /// Copy during cloning
  virtual Space*
  copy(void) {
    return new Melody(*this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {

    os << "\t";
    for (int i = 0; i<bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(push[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(pull[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(playing[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl;
  }
};

/** \brief Main-function
 *  \relates melody
 */
int main(int argc, char* argv[]) {
  SizeOptions opt("Melody");
  opt.solutions(1);
  opt.parse(argc,argv);
  Script::run<Melody,DFS,SizeOptions>(opt);
  return 0;
}
