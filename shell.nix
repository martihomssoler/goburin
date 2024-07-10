{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell rec {
    nativeBuiltInputs = with pkgs; [
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXi
      xorg.libX11
      pkg-config
      libGL
      libGLU
      python3
    ];
    buildInputs = with pkgs; [
      clang
      llvmPackages.bintools
      rustup
      # EGUI + WINIT bug fixing #
      xorg.libX11
      wayland
      libxkbcommon
      # - #
      fasm
      stdenv.cc
      stdenv.cc.libc stdenv.cc.libc_dev
    ];
    RUSTC_VERSION = "stable";
    # https://github.com/rust-lang/rust-bindgen#environment-variables
    LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];
    shellHook = ''
      export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
      export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
      export LD_LIBRARY_PATH=/run/opengl-driver/lib/:${with pkgs; lib.makeLibraryPath ([ 
        wayland 
        libxkbcommon 
        libGL 
        libGLU
        xorg.libX11
        xorg.libXcursor
        xorg.libXi
        xorg.libXrandr
        vulkan-loader
      ])}
      '';
    # Add precompiled library to rustc search path
    RUSTFLAGS = (builtins.map (a: ''-L ${a}/lib'') [
      # add libraries here (e.g. pkgs.libvmi)
    ]);
    # Add glibc, clang, glib, and other headers to bindgen search path
    BINDGEN_EXTRA_CLANG_ARGS = 
    # Includes normal include path
    (builtins.map (a: ''-I"${a}/include"'') [
      # add dev libraries here (e.g. pkgs.libvmi.dev)
      pkgs.glibc.dev 
    ])
    # Includes with special directory paths
    ++ [
      ''-I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
      ''-I"${pkgs.glib.dev}/include/glib-2.0"''
      ''-I${pkgs.glib.out}/lib/glib-2.0/include/''
    ];

    # buildInputs_path = pkgs.lib.makeLibraryPath buildInputs;
    # wayland_path = pkgs.lib.makeLibraryPath pkgs.wayland;
    # libxkbcommon_path = pkgs.lib.makeLibraryPath pkgs.libxkbcommon;
    # LD_LIBRARY_PATH = "$LD_LIBRARY_PATH:${buildInputs_path}:${wayland_path}:${libxkbcommon_path}";

    packages = [
      (pkgs.python3.withPackages (python-pkgs: [
        python-pkgs.pandas
        python-pkgs.requests
      ]))
    ];
  }
