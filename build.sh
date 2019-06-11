#!/bin/bash
set -e
SECONDS=0

# ============================================================================
# 
# SHABAKA
# Image-Based Mesh Generation
# ----------------------------------------------------------------------------
# 
# Read installation instructions in README before running this script!
# 
# This script must be run using sudo. Most commands do not use superuser
# privileges, but some require it.


# ============================================================================
#
# DETERMINE OS
#
# ----------------------------------------------------------------------------

UNAME_S=$(uname -s)
NAME=$SUDO_USER
NOTSUDO="sudo -u $NAME" #RUN AS NORMAL USER
shabaka_dir=`pwd`;
rm -rf src/external *.tgz *.exe


# ----------------------------------------------------------------------------
# MAC
# ----------------------------------------------------------------------------

if [ $UNAME_S == "Darwin" ]; then
   # SETUP
   $NOTSUDO echo "Updating Homebrew..."
   $NOTSUDO brew update
   sudo chmod +t /tmp
   sudo chmod +t /usr/local/Cellar/
   sudo chmod +t ~/Library/Caches/Homebrew/

   # GCC 6.3.0
   $NOTSUDO brew reinstall gcc
   
   # GNU SED
   $NOTSUDO brew reinstall gnu-sed
   
   # WGET 1.14
   $NOTSUDO brew reinstall wget
   
   # CMAKE 3.7.2
   $NOTSUDO brew reinstall cmake  
   
   # VTK 7.1.0
   $NOTSUDO brew reinstall vtk

   # TEEM 1.11.0
   $NOTSUDO brew reinstall teem

   # NLOPT 2.4.2
   $NOTSUDO brew reinstall nlopt

   # QHULL 2015.2
   $NOTSUDO brew reinstall qhull

   # ACVD 2.0
   $NOTSUDO git clone https://github.com/valette/ACVD src/external/ACVD
   cd src/external/ACVD
   $NOTSUDO cmake . -DCMAKE_BUILD_TYPE=Release -DVTK_DIR=/usr/local/Cellar/vtk/8.2.0
   $NOTSUDO LANG=C sed -i -e 's/vtkstd\/string/vtkStdString/g' VolumeProcessing/vtkOOCMetaImageReader.cxx
   $NOTSUDO LANG=C sed -i -e 's/vtkstd::string/vtkStdString/g' VolumeProcessing/vtkOOCMetaImageReader.cxx
   $NOTSUDO make -j4
   cd -

   # MESHLAB 2016.12
   if [ -d /Applications/meshlab.app ]; then
      sudo rm -rf /Applications/meshlab-old.app
      sudo mv /Applications/meshlab.app /Applications/meshlab-old.app
   fi
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/mac/MeshLab2016.12.dmg"
   $NOTSUDO yes | hdiutil attach -nobrowse MeshLab2016.12.dmg > /dev/null
   $NOTSUDO cp -R /Volumes/MeshLab2016.12/meshlab.app/ /Applications/meshlab.app/
   cd /Applications/meshlab.app/Contents/MacOS/
   $NOTSUDO install_name_tool -add_rpath "@executable_path/../Frameworks" meshlabserver
   cd -
   $NOTSUDO hdiutil unmount /Volumes/MeshLab2016.12
   sudo rm -rf MeshLab2016.12.dmg*

   # PARAVIEW 5.3.0
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/mac/ParaView-5.3.0-Qt5-OpenGL2-MPI-OSX10.8-64bit.dmg"
   $NOTSUDO yes | hdiutil attach ParaView-5.3.0-Qt5-OpenGL2-MPI-OSX10.8-64bit.dmg > /dev/null
   sudo rm -rf /Applications/ParaView-5.3.0.app
   $NOTSUDO cp -R /Volumes/ParaView-5.3.0-Qt5-OpenGL2-MPI-OSX10.8-64bit/ParaView-5.3.0.app/ /Applications/ParaView-5.3.0.app
   $NOTSUDO hdiutil unmount /Volumes/ParaView-5.3.0-Qt5-OpenGL2-MPI-OSX10.8-64bit
   rm -rf ParaView-5.3.0*

   # SEG3D 2.3.2
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/mac/Seg3D2-2.3.2-Darwin.dmg"
   $NOTSUDO hdiutil mount -nobrowse Seg3D2-2.3.2-Darwin.dmg
   sudo cp -R /Volumes/Seg3D2-2.3.2-Darwin .
   sudo installer -package Seg3D2-2.3.2-Darwin/Seg3D2-2.3.2-Darwin.pkg -target /
   $NOTSUDO hdiutil unmount /Volumes/Seg3D2-2.3.2-Darwin
   sudo rm -rf Seg3D2-2.3.2-Darwin Seg3D2-2.3.2-Darwin.dmg*
   
   # TETGEN 1.5.0
   $NOTSUDO brew reinstall tetgen  
   
   # GMSH 2.16
   $NOTSUDO brew reinstall gmsh
   
   # SHABAKA
   export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/libexec:$LD_LIBRARY_PATH
   
   $NOTSUDO make clean -C $shabaka_dir/src/ptcloudgen/
   $NOTSUDO make clean -C $shabaka_dir/src/surfrecon/voronoi/vsitegen/
   $NOTSUDO make clean -C $shabaka_dir/src/surfrecon/voronoi/qvor2vtk/
   $NOTSUDO make clean -C $shabaka_dir/src/pmeshgen/ply2smfe/
   
   $NOTSUDO make -C $shabaka_dir/src/ptcloudgen/
   $NOTSUDO make -C $shabaka_dir/src/surfrecon/voronoi/vsitegen/
   $NOTSUDO make -C $shabaka_dir/src/surfrecon/voronoi/qvor2vtk/
   $NOTSUDO make -C $shabaka_dir/src/pmeshgen/ply2smfe/
   
   echo ""
   echo "------------------------------------------------------------------------------ "
   echo "ALL DONE!"
   elapsed=$SECONDS
   export elapsed
   echo "import os; print 'ELAPSED TIME:', format(float(os.environ['elapsed'])/60., '.2f'), 'minutes'" | python
   echo ""
  
# ----------------------------------------------------------------------------
# LINUX
# ----------------------------------------------------------------------------

elif [ $UNAME_S == "Linux" ]; then
   MYVERSION=$(lsb_release -r | awk '{print $2}')

   # SETUP
   sudo apt-get -y install build-essential libpython2.7-dev libproj-dev libxcursor-dev
   sudo apt-get -y install libpng-dev libjpeg-dev libxxf86vm1 libboost-all-dev
   sudo apt-get -y install libxxf86vm-dev libxi-dev libxrandr-dev libtinyxml-dev
   sudo apt-get -y install freeglut3-dev mesa-common-dev mesa-utils-extra libgdcm-tools
   $NOTSUDO mkdir src/external
   export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:/usr/local/lib32:/usr/local/libexec:$LD_LIBRARY_PATH

   # GCC 4.8.4
   sudo apt-get -y install gcc g++ gfortran

   # WGET 1.15
   sudo apt-get -y install wget

   # CMAKE 2.8.12
   sudo apt-get -y install cmake  
   
   # VTK 6.0
   sudo apt-get -y install vtk6 libvtk6-dev

   # TEEM 1.11.0
   sudo apt-get -y install libteem-dev teem-apps
   sudo ln -sf /usr/bin/teem-unu /usr/bin/unu

   # NLOPT 2.4.1
   sudo apt-get -y install libnlopt-dev

   # QHULL 2015.2
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/linux/qhull-2015-src-7.2.0.tgz"
   $NOTSUDO tar -xvf qhull-2015-src-7.2.0.tgz -C src/external
   $NOTSUDO make -C src/external/qhull-2015.2
   $NOTSUDO rm qhull-2015-src-7.2.0.tgz*  

   # ACVD 2.0
   $NOTSUDO git clone https://github.com/valette/ACVD src/external/ACVD
   cd src/external/ACVD
   export LD_LIBRARY_PATH=/usr/local
   if [ $MYVERSION == "16.04" ]; then
      sudo ln -sf /usr/bin/vtk6 /usr/bin/vtk
   fi
   $NOTSUDO cmake . -DCMAKE_BUILD_TYPE=Release
   $NOTSUDO sed -i -e 's/vtkstd\/string/vtkStdString/g' VolumeProcessing/vtkOOCMetaImageReader.cxx
   $NOTSUDO sed -i -e 's/vtkstd::string/vtkStdString/g' VolumeProcessing/vtkOOCMetaImageReader.cxx
   $NOTSUDO make -j4
   cd -

   # PARAVIEW 4.0.1
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/linux/ParaView-4.0.1-Linux-64bit.tgz"
   $NOTSUDO tar -xvf ParaView-4.0.1-Linux-64bit.tgz -C src/external
   $NOTSUDO rm ParaView-4.0.1-Linux-64bit.tgz*
   
   # TETGEN 1.5.0
   sudo apt-get -y install tetgen
   
   # GMSH 3.0.1
   $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/linux/gmsh-3.0.1-Linux64.tgz"
   $NOTSUDO tar -xvf gmsh-3.0.1-Linux64.tgz -C src/external
   $NOTSUDO rm gmsh-3.0.1-Linux64.tgz*
   
   # SHABAKA
   $NOTSUDO make clean -C $shabaka_dir/src/ptcloudgen/
   $NOTSUDO make clean -C $shabaka_dir/src/surfrecon/voronoi/vsitegen/
   $NOTSUDO make clean -C $shabaka_dir/src/surfrecon/voronoi/qvor2vtk/
   $NOTSUDO make clean -C $shabaka_dir/src/pmeshgen/ply2smfe/
   
   $NOTSUDO make -C $shabaka_dir/src/ptcloudgen/
   $NOTSUDO make -C $shabaka_dir/src/surfrecon/voronoi/vsitegen/
   $NOTSUDO make -C $shabaka_dir/src/surfrecon/voronoi/qvor2vtk/
   $NOTSUDO make -C $shabaka_dir/src/pmeshgen/ply2smfe/
   echo ""
   
   if grep -q Microsoft /proc/version; then
      ## WINDOWS WSL
      
      # MESHLAB 2016.12
      sudo add-apt-repository -y ppa:zarquon42/meshlab
      sudo apt-get -y update
      sudo apt-get -y install meshlab
      
      # DOWNLOAD WINDOWS EXECUTABLES
      $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/windows/Seg3D2-2.3.2-win64.exe"
      $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/windows/MeshLab2016.12.exe"
      $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/windows/ParaView-5.2.0-Qt4-OpenGL2-Windows-64bit.exe"
      $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/windows/Xming-6-9-0-31-setup.exe"
      $NOTSUDO mkdir src/external/windows 
      $NOTSUDO mv *.exe src/external/windows

      echo "------------------------------------------------------------------------------ "
      echo "FINISHED BUILD. PLEASE COMPLETE REMAINING INSTRUCTIONS IN README!"
      elapsed=$SECONDS
      export elapsed
      echo "import os; print 'ELAPSED TIME:', format(float(os.environ['elapsed'])/60., '.2f'), 'minutes'" | python
      echo ""
   
   else
      ## UBUNTU
      
      # MESHLAB 2016.12
      sudo add-apt-repository -y ppa:zarquon42/meshlab
      sudo apt-get -y update
      sudo apt-get -y install meshlab
#       sudo apt-get -y install snapd
#       sudo snap install meshlab
   
      # SEG3D 2.1.4
      if [ $MYVERSION == "14.04" ]; then
         sudo apt-get -y install libinsighttoolkit3.20 libtet1.5
         $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/linux/seg3d_2.1.4-0ubuntu0ppa3_amd64.deb"
         sudo dpkg -i seg3d_2.1.4-0ubuntu0ppa3_amd64.deb
         sudo rm seg3d_2.1.4-0ubuntu0ppa3_amd64.deb*
      fi  
         
      # ADD DESKTOP FILES
      $NOTSUDO wget "https://github.com/omhafez/shabaka-externals/raw/master/linux/desktop.tgz"
      $NOTSUDO tar -xf desktop.tgz
      sudo mv desktop/*.png /usr/share/icons
      sudo mv desktop/*.desktop ~/.local/share/applications
      $NOTSUDO rm -rf desktop desktop.tgz*
         
      echo "------------------------------------------------------------------------------ "
      echo "ALL DONE!"
      elapsed=$SECONDS
      export elapsed
      echo "import os; print 'ELAPSED TIME:', format(float(os.environ['elapsed'])/60., '.2f'), 'minutes'" | python
      echo ""

   fi
   
   
# ----------------------------------------------------------------------------
# UNSUPPORTED OS
# ----------------------------------------------------------------------------

else
   $NOTSUDO echo "ERROR: unrecognized operating system" >&2
fi
