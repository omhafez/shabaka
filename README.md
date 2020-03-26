------------------------------------------------------------------------------
# **SHABAKA**  
## Image-Based Mesh Generation

by Omar Hafez and Mark Rashid  
University of California, Davis  

https://github.com/omhafez/shabaka  
https://www.linkedin.com/in/omarhafez34  
omhafez@ucdavis.edu  

------------------------------------------------------------------------------
![alt text](https://raw.githubusercontent.com/omhafez/shabaka-externals/master/shabaka.png "shabaka workflow png")  

------------------------------------------------------------------------------

**Shabaka** is a command-line tool that reads segmented images and generates
watertight surface meshes (or b-reps) of the objects of interest. Surface
meshes can be piped into mesh generation tools for scientific computing,
3D printing programs, or visualization software. Shabaka is fast and easy to
install and use. Little to no prior knowledge of Unix is required. Meshes are
generated in a matter of minutes, making it ideal for processing large
datasets of segmented images.  

"Shabaka" means *mesh* in Arabic, and is the name of a black Egyptian pharaoh
who ruled in the 8th century BC.  

------------------------------------------------------------------------------

## **INSTALLATION**  

Installation requires an internet connection. The code has been tested on Mac OS Mojave
10.14.5, Ubuntu 19.10, and Windows 10 Creators Update (OS build 15063) on 64-bit systems.
It is recommended that you upgrade your system appropriately. Instructions for updating
your Windows system is included below.  


### INSTRUCTIONS FOR MAC *(15 minutes)*

‣ Type Command(⌘)+Space bar to open Spotlight Search, type Terminal, and hit
return. Make the following call in terminal:  
~~~~
xcode-select --install
~~~~
If you get an error saying command line tools are already installed, just move
on to the next step. Otherwise, click Install, then Agree.  

‣ Documented issues with developer tools for MacOS Mojave require you to remove Xcode.app
during installation (click [here](https://github.com/PointCloudLibrary/pcl/issues/2601#issuecomment-486889884)
for more details). Remove Xcode with the call below (followed by your Mac password). You may install the app again after installation from the App Store.
~~~~
sudo rm -rf /Applications/Xcode.app
~~~~

‣ Install Homebrew:
~~~~
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
~~~~

‣ Update packages and install python and git:
~~~~
brew update && brew upgrade
brew install python@2
brew install git
~~~~

‣ Assuming you want Shabaka in your home directory, make the following
calls in the terminal (ignore warnings):  
~~~~
cd $HOME
git clone https://github.com/omhafez/shabaka.git
cd shabaka
echo 'export shabaka_dir='$PWD >> ~/.bash_profile
cat .paths/macpaths >> ~/.bash_profile && source ~/.bash_profile
~~~~

‣ Enter the following call (followed by your Mac password) to run the build script:  
~~~~
sudo ./build.sh
~~~~
If Homebrew gives any installation errors, just follow the instructions they
provide to resolve the issue and run                                          `
sudo ./build.sh                                                               `
again.  

‣ The build script installs the external applications Meshlab 2016.12,
Paraview 5.3.0, and Seg3D 2.4.4. The calls appending *~/.bash_profile*
make them the default versions of those applications.  

‣ Any issues installing will be minor.
Please email omhafez@ucdavis.edu for help.  


### INSTRUCTIONS FOR UBUNTU *(15 minutes)*

‣ Open a terminal and make the following call to install git:  
~~~~
sudo apt-get -y install git
~~~~

‣ Assuming you want Shabaka in your home directory, make the following
calls in the terminal (ignore warnings):  
~~~~
cd $HOME
git clone https://github.com/omhafez/shabaka.git
cd shabaka
echo 'export shabaka_dir='$PWD >> ~/.bashrc
cat .paths/linuxpaths >> ~/.bashrc && source ~/.bashrc
sudo ./build.sh
~~~~
‣ If you come across any errors (particularly from *apt-get*), attempt the
following calls and try again:
~~~~
sudo apt-get -y update && sudo apt-get -y upgrade
sudo apt-get -y dist-upgrade && sudo apt-get -f install && sudo apt-get -y autoremove
~~~~
‣ The build script installs the external applications Meshlab 2016.12 and
Paraview 4.0.1. The calls appending *~/.bashrc* make them the default versions
of those applications.

‣ Seg3D is not a part of the installation, but can be built from source from
the following link: https://github.com/SCIInstitute/Seg3D/releases/latest.
If you (understandably) don't want to build Seg3D from source, you may use
Slicer (http://slicer.org) instead, which provides prebuilt executables for
Linux.

‣ If you have any issues installing, they are certain to be minor.
Please email omhafez@ucdavis.edu for help.  


### INSTRUCTIONS FOR WINDOWS *(30 minutes)*

‣ Click the Start menu and search "About your PC". Ensure that you are running
Windows 10, Version 1703, OS build 15063 (or newer). If you are not, go to
the following link to update your version of Windows 10 to the Creators
Update (allocate an additional 30 minutes for this task):
https://www.microsoft.com/en-us/software-download/windows10  

‣ Follow these instructions to get Linux Bash Shell on Windows:  
https://www.howtogeek.com/249966/how-to-install-and-use-the-linux-bash-shell-on-windows-10/  
Please make the UNIX username and password match that of your native Windows
account. Ignore the portion of the article that discusses fonts.

‣ Start the "Bash on Ubuntu on Windows" application. Make the following call:
~~~
sudo rm /etc/hosts
~~~

‣ Close the "Bash on Ubuntu on Windows" application and reopen it. Make the following calls:  
~~~~
sudo apt-get -y update && sudo apt-get -y upgrade
sudo apt-get -y dist-upgrade && sudo apt-get -f install && sudo apt-get -y autoremove
~~~~

‣ Make the following call to install git:  
~~~~
sudo apt-get -y install git
~~~~

‣ Let's assume you want Shabaka in your home directory on Windows. A symbolic
link will be placed in the home directory of the Linux subsystem as well. Make the
following calls in the terminal (ignore warnings):  
~~~~
cd /mnt/c/Users/$USER
git clone https://github.com/omhafez/shabaka.git
cd $HOME && ln -sf /mnt/c/Users/$USER/shabaka
cd shabaka
echo 'export shabaka_dir='$PWD >> ~/.bashrc
cat .paths/windowspaths >> ~/.bashrc && source ~/.bashrc
sudo ./build.sh
~~~~

‣ Finish Windows build with the following steps:

* Open File Explorer by typing Windows + E  
* Copy the following into the file path:                                      `
  C:\Users\%USERNAME%\shabaka\src\external\windows                            `
* Install Meshlab, Paraview, and Seg3D as administrator. Install any
  intermediate software that may pop up along the way. Leave default
  installation options as is.  
* Install Xming as administrator. Leave default installation options as
  is.
* **Launch Xming**  
* Type Windows --> search for Xming --> right click --> Open file location  
* Copy the Xming shortcut  
* Open the Run window by typing Windows + R, and run the following:          `
  shell:common startup                                                       `
* Paste the Xming shortcut to this location

‣ Make a desktop shortcut with the following steps:

* Right click on Desktop --> New --> Shortcut  
* Paste the following location:                                               `
  C:\Users\%USERNAME%\shabaka                                                 `

‣ The build script installs the external applications Meshlab 2016.12 and
Paraview 4.0.1 in the Linux subsystem. The calls appending *~/.bashrc*
make them the default versions within the subsystem.  

‣ If you have any issues installing, they are certain to be minor.
Please email omhafez@ucdavis.edu for help.  

------------------------------------------------------------------------------

## **SETUP**  

Assuming you are in the *shabaka* directory, type the following commands:  
~~~~
cd reslt
mkdir my-new-mesh
cd my-new-mesh
~~~~
Copy an image mask (segmentation) in .nrrd format into the current directory,
and you are ready to begin.
See *doc/segmentation.pdf* for a full tutorial on how to segment images.  

##### If your image mask is not in NRRD format:  
+ Open Seg3D and import your mask into Seg3D and then select
  File --> Export Segmentation and save as .nrrd  
+ Seg3D provides Python scripting capability in the event that you need to
  convert several images.  
+ Slicer (http://slicer.org) and ITK-Snap (http://itksnap.org) can also convert
  raw and/or segmented data to the .nrrd format  
+ Otherwise, there are several command line image converter tools available
  online. Please contact omhafez@ucdavis.edu with any difficulty in converting
  to the .nrrd format.  

------------------------------------------------------------------------------

## **USAGE**  

### TO CALL:
`
shabaka mask.nrrd [options]
`  
where *mask.nrrd* is an image segmentation (mask) provided by the user.  

#### Options:   
* -nXXX: specify desired surface points following surface
      reconstruction step, denoted here as XXX. The number of surface points
      should typically be on the order of 25000.  
* -dYYY: specify Voronoi site pair distance in number of voxels (for
      Voronoi-based surface reconstruction), denoted here as YYY. Typically
      not to be changed.  
* -tZZZ: call tetgen to produce tetrahedral volume mesh from surface generated
      by Shabaka. The string ZZZ is the set of tetgen options (Refer to
      http://wias-berlin.de/software/tetgen). If ZZZ is not specified,
      default tetgen options are -pqY  
* -k: output tetrahedral volume mesh in VTK format. Requires -t flag to be
      called as well  
* -b: output tetrahedral volume mesh in Abaqus .inp format. Requires -t flag
      to be called as well  
* -l: (lowercase L) generate linear tetrahedra (default is quadratic).
      Requires -t flag to be called as well  
* -v: run Shabaka in verbose mode  
* -a: export all intermediate files  
* -s: perform smoothing on point cloud (should not typically be necessary)  
* -p: perform Screened Poisson surface reconstruction instead of default
      Voronoi-based surface reconstruction *(not available on Ubuntu,
      pending bug fixes in Meshlab)*  
* -f: overwrite any previously existing Shabaka output files  

Options should be separated by a space, and should have no whitespace
between the flag and value, exactly as shown in the example call.  

#### RECOMMENDED CALL:  
`
shabaka mask.nrrd -n30000 -d1.1 -tpqY -k -b
`  
Generates 30,000 surface points, uses Voronoi site pair distance of 1.1 voxels,
and generates quadratic tetrahedra in Tetgen, VTK, and Abaqus formats.
Tetgen options are -pqY.

#### Other examples:
`
shabaka mask.nrrd
`  
(Default) Generates 30,000 surface points and uses Voronoi site pair distance
of 1.1 voxels. No tetrahedral meshing, verbose mode off, no intermediate files
exported, point cloud smoothing off, and Voronoi-based surface reconstruction
performed.

`
shabaka mask.nrrd -s -v -l -t -k
`  
Generates 30,000 surface points, uses Voronoi site pair distance of 1.1 voxels,
smooths point cloud, verbose mode on, generates linear tetrahedra in
Tetgen and VTK formats. Tetgen options are -pqY.

`
shabaka mask.nrrd -n25000 -p -a -f
`  
Generates 25,000 surface points, performs Screened Poisson surface
reconstruction, and exports all intermediate files. Overwrites previously
existing Shabaka output files. *(Mac and Windows only)*


#### Additional capability:
`
shabaka mask-ptcloud.ply -c0.4 -tpqY -k -b
`  
Shabaka can also mesh input oriented point clouds directly. Ensure that
the file is in PLY format, includes both point and normal data, and the file
name ends in "-ptcloud.ply", for example, heart-ptcloud.ply. The additional
-c flag is required to provide the voxel spacing from the original image. If
that value is not available, divide the length of your object by about
250 for a rough estimate.


### WALL CLOCK TIME:
Surface meshes can be generated for the provided datasets in less than 5
minutes on a 4 x 2.4 GHz CPU. Images involving complex interfaces or in-plane
resolutions higher than 256 x 256 pixels may take slightly more time.  


### OUTPUT:  
###### The following files are generated upon calling Shabaka:  
* mask.ply - watertight b-rep in Stanford PLY format  
* mask.stl - same file in STL format  

###### If -t is flag included, additionally:  
* mask-tetgen - subdirectory that includes all tetgen output files  

###### If -k is flag included, additionally:
* mask.vtk - tetrahedral mesh in legacy VTK format

###### If -b is flag included, additionally:
* mask.inp - tetrahedral mesh in Abaqus .inp format

###### If -a is flag included, additionally:  
* mask-resampled.nrrd - resampled and padded image mask used for point cloud
  generation
* mask-ptcloud.ply - point cloud used for surface reconstruction, in PLY
  format  
* mask.xyz - Voronoi sites in XYZ format  
* mask.vor - Voronoi partition output from Qhull  
* mask-fine.ply - triangulated b-rep prior to surface decimation in PLY
  format  

Results may be inspected in Meshlab and/or Paraview. They can be opened from
the Start menu, or alternatively, if using Mac or Linux, by opening a terminal
and typing `meshlab` to open Meshlab and `paraview` to open Paraview.

------------------------------------------------------------------------------

## **NOTES**  

### IMPORTANT  

- Shabaka can mesh multiple disjoint objects within a single image, but they
  must belong to the same mask label. Meshing multi-label masks or multiple
  masks at once has not been implemented.

- It is recommended that your image mask has at least 256 voxels in at least
  two directions, i.e., your slice resolution is at least 256 × 256 pixels.
  Generally speaking, more voxels leads to better (and slower) results. Say
  your current resolution is *nx* × *ny* × *nz* and you want to resample
  your image and/or image mask to *Nx* × *Ny* × *Nz*. Simply enter in the
  command line: `  
  unu resample -s x<f1> -x<f2> -x<f3> -i mask.nrrd -o newmask.nrrd          `  
  where *f1* = *Nx*/*nx*, *f2* = *Ny*/*ny*, and *f3* = *Nz*/*nz*. For
  example,                                                                  `
  unu resample -s x1.2 -x2.4 -x1.0 -i mask.nrrd -o newmask.nrrd             `  

- It is assumed that the calls to shabaka are correct. Error handling is not
  fully developed.  

- If a region of the surface mesh exhibits unexpected behavior, there likely
  were not enough points in the point cloud generated in that region. You may
  run Shabaka with the -a flag and inspect mask-ptcloud.ply to verify this.
  This is likely due to a poor segmentation in that region. Attempt the
  following to resolve the issue:  
    + Fix/smooth the segmentation in the location where the mesh exhibits
      undesirable behavior. Ensure that features in the image mask are at
      least 3-5 voxels wide.  
    + If meshing a smooth object, you may run the problem with the -s option
      for a smooth point cloud. That is, make the following call:             `
      shabaka mask.nrrd -nXXX -dYYY -s                                        `  
      where XXX and YYY are the same values as the previous run.
    + You may also attempt to change the value of the Voronoi site distance.
      That is, make the following call:                                       `
      shabaka mask.nrrd -nXXX -dYYY -s                                        `  
      where YYY is now a different value than the previous.  
      ⋅     
- If for whatever reason the generated surface mesh is still not satisfactory,
  run Shabaka with the -p flag to use Screened Poisson Surface Reconstruction
  instead, which may perform more robustly in certain circumstances. Note that
  this will generate meshes that are sometimes unnecessarily smoother than
  the default Voronoi approach.  


### ADDITIONAL COMMENTS  

- A suite of example segmented images have been provided. If you would also
  like the corresponding raw image data (~1.25 GB), make the following calls:
   ~~~~
   wget https://github.com/omhafez/shabaka-examples/releases/download/v1.0/shabaka-examples.tgz
   tar -xf shabaka-examples.tgz -C $shabaka_dir/reslt
   rm -rf shabaka-examples.tgz
   ~~~~

- If you need to exit the code prematurely, you may need to type ctrl-c
  several times. Alternatively, type ctrl-z or close the terminal.  

- Always make a new directory for each mesh you want to make. If you are
  generating several meshes for the same input data, just link to the same
  input file.

- Ignore warnings and errors you may encounter when running in verbose mode,
  particularly from Meshlab.  

- Having more than one instance of Shabaka on your system is not
  recommended.  


### WORKING WITH SOFTWARE UPSTREAM AND DOWNSTREAM OF SHABAKA  

- Shabaka takes as input a segmented image, which can be generated in the
  free software Seg3D (included in this build). Open Seg3D and refer to
  *doc/segmentation.pdf* for a tutorial on how to generate segmented
  images (i.e., image masks) from raw image data.  

- The build script for Shabaka also includes installation of Meshlab,
  Paraview, and tetgen, all of which may come in handy in viewing and using
  the ouput of this code.

- If you are using Windows, the packages Meshlab, Paraview, and Seg3D are
  installed on the *Windows system* in addition to the *Linux subsystem* for
  pre- and post-processing purposes.  

- If you desire an output mesh other than .vtk or .inp, use Gmsh (a part
  of the installation) to convert to a number of other file formats. Gmsh
  can be run through the command line if preferred. Please contact
  omhafez@ucdavis.edu if you would like a particular output mesh file
  format implemented.  

------------------------------------------------------------------------------

Shabaka is the proprietary property of The Regents of the University of California.

Copyright © 2017 The Regents of the University of California, Davis campus. All Rights Reserved.

This software may be patent pending.

The software program and documentation are supplied "as is", without any accompanying services from The Regents, for purposes of confidential discussions only. The Regents does not warrant that the operation of the program will be uninterrupted or error-free. The end-user understands that the program was developed for research purposes and is advised not to rely exclusively on the program for any reason.

IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THE REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE REGENTS HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
