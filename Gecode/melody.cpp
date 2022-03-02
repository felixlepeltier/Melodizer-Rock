#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/int/arithmetic.hh>
#include <gecode/set.hh>
#include <vector>

using namespace Gecode;

class Melody : public Script {
private:

  const int bars = 4;
  const int quantification = 8;

  SetVarArray push;
  SetVarArray pull;

public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127),
    pull(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127){

      SetVarArray playing(*this,bars*quantification,IntSet::empty,IntSet(0,127),0,127);

      rel(*this, pull[0] == IntSet::empty);

      for(int i = 1; i < bars*quantification; i++){
        rel(*this, playing[i] == (playing[i-1] - pull[i]) | push[i]); // Notes that are playing
        rel(*this, pull[i] <= playing[i-1]); // Cannot pull a note not playing
        rel(*this, push[i] || (playing[i-1] - pull[i])); // Cannot push a note still playing
      }

      for(int i = 1; i < bars*quantification; i++){
        cardinality(*this, playing[i], 0, 3);
      }

      branch(*this, push, SET_VAR_NONE(), SET_VAL_MIN_INC());
      branch(*this, pull, SET_VAR_NONE(), SET_VAL_MIN_INC());

  }

  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody){

      push.update(*this, melody.push);
      pull.update(*this, pull.end);

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
      os << push[i] << ", " << pull[i];
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
