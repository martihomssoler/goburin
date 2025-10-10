set export

src_dir_name := "src"
build_dir_name := "build"
in_extension := "asm"

@build: setup
    fasm $src_dir_name"/goburin_"$in_extension"."$in_extension $build_dir_name"/goburin_"$in_extension

@run: build
    "./"$build_dir_name"/goburin_"$in_extension

# ---------------------------------------------

wbuild:
    watchexec -c -e asm -r -- just build 

wrun:
    watchexec -c -e asm -r -- just run

whex arg:
    watchexec -c --exts bin -- hexyl {{arg}}

# ---------------------------------------------

[private]
@setup:
    mkdir -p $build_dir_name 
