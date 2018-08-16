Por munti en unikseca operaciumo, estas facile:

1. Instali fpc-on (ekz-e per apt-get, dnf, pkg\_add ktp)
2. Instali git-on kaj svn-on
3. Instali gmake-on (make-on de GNU)
4. Install la retan bibliotekon Synapse:

    mkdir ~/.fpc_units

    cd ~/.fpc_units

    svn co https://svn.code.sf.net/p/synalist/code/trunk synapse

    echo "" >> ~/.fpc.cfg

    echo "# Uzi la Synapse-unuojn" >> ~/.fpc.cfg

    echo "-Fu~/.fpc_units/synapse" >> ~/.fpc.cfg

5. Instali R3R (vidu malsupre por kiel atingi la fontotekstojn per svn)

    git clone https://gitlab.com/keithbowes/r3r.git

    cd r3r

    gmake

    sudo -E gmake install

    a. Por instali en la hejman dosierujon:

        gmake install PREFIX=$HOME

Por la grafika fasado:

1. 
    a. Instali la bibliotekon wxWidgets (ekz-e en Mandrivo/Magejo: sudo dnf lib64wxgtku3.0-devel)
    b. Se ne disponebla en via operaciumo:

        cd ..

        git clone https://github.com/wxWidgets/wxWidgets

        cd wxWidgets

        mkdir build

        cd build

        ../configure --prefix=/usr/local

        gmake

        sudo gmake install

        cd ../..

        cd r3r

    c.

        Simile kiel supre vi povas uzi la argumenton --prefix=$HOME por instali en vian hejman dosierujon.
2. Kompilumi kun grafika fasado

    gmake R3R_UI=wx

    [sudo -E] gmake install R3R_UI=wx [PREFIX=$HOME]

En Vindozo la procedo estas simila sed pli malsimpla, ĉar Vindozo ne havas tian medion. Vi devas tial instali uniksecan tavolon.  Tiaj tavoloj estas [Cygwin](http://cygwin.com/), [MSYS](http://mingw.org/wiki/msys), inter aliaj.
