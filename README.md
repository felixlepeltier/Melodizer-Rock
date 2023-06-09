# Melodizer Rock

**Melodizer Rock** is a library for [OpenMusic](https://openmusic-project.github.io/) extending the previous library [Melodizer 2.0](https://github.com/clemsky/TFE-Composition-Musicale) with an interface to build rock songs.

## Installation
**Warning**: the installation can only be done on MacOS or Linux due to some icompatibility of some tools with other OS.

To install **Melodizer Rock**, you must first install the following tools:
- [**Gecode**](https://www.gecode.org/) a library for constraint programming used by all version of Melodizer
- [**GiL**](https://github.com/sprockeelsd/GiL) a wrapper that allows the use of Gecode function in Lisp programs. Once cloned, switch to the branch *melodizer-rock-bab*
- [**OpenMusic**](https://openmusic-project.github.io/) the application for which **Melodizer Rock** is built.

Once those tools have been installed, **Melodizer Rock** can be installed by following those steps:
- Clone this repository
- Open OpenMusic and open a workspace
- In the taskbar, click on "Windows" and then "Library", or simply press \texttt{Shift+Ctrl+P}
- In the taskbar, click on "File" and then "Add Remote User Library"
- Navigate to both GiL and Melodizer Rock's folders and add them

If you wish to load these libraries by default into OpenMusic, to avoid this tedious library loading process each time you launch OpenMusic, then follow these steps:
- Launch OpenMusic
- Enter an existing workspace, or create a new workspace
- In the taskbar, click on "OM 7.1" and then "Preferences", or simply press \texttt{Ctrl+,}
- In the pop-up, click on the "Libraries" tab
- Click on the folder icon
- Navigate to both GiL and Melodizer Rock's folders and add them
- Click on "Apply"
- Check the boxes next to both GiL and Melodizer in the "Auto Load" box

## Structure
This will explain the different parts of this repository and point out where the **Melodizer Rock** files are located.

#### Gecode
The Gecode folder contains examples on how to use the Gecode library. **Melodizer Rock** did not add anything to this folder.

#### Melodizer-old
The Melodizer-old folder contains the first implementation of [Melodizer](https://github.com/sprockeelsd/Melodizer).

#### Melodizer-main
The Melodizer-main folder contains the files used for the current Melodizer library, including those of Melodizer 2.0.

##### Melodizer/sources
This folder contains all the functions, object and interfaces implementation of **Melodizer Rock**:
1. *melodizer-xxx*.lisp are the files of Melodizer 2.0
2. *block.lisp* is the file containing the Block object of Melodizer 2.0
3. *rock-xxx.lisp* are the files of **Melodizer Rock**
    - *rock.lisp*: Rock object, representing a full song, and its interface definition
    - *rock-AB.lisp*: A and B objects and their interface definitions
    - *rock-srdc.lisp*: the s r d and c object and their interface definitions
    - *rock-accompaniment*: the accompaniment object and its panel definition
    - *rock-csp.lisp*: the functions building the csp after the structure the composer built in the interface
    - *rock-csts.lisp*: the function posting the constraints as specified by the composer through the interfaces
    - *rock-utils.lisp*: the utility functions used throughout **Melodizer Rock**

[Check out our poster !](PosterV2.pdf)
