set export

src_dir_name := "src"
build_dir_name := "build"
in_extension := "asm"
out_extension := "forth"

@build: setup
    fasm $src_dir_name"/goburin_"$in_extension"."$in_extension $build_dir_name"/goburin_"$in_extension".o"
    ld -o $build_dir_name"/goburin_"$in_extension $build_dir_name"/goburin_"$in_extension".o"
    rm -f -- $build_dir_name"/goburin_"$in_extension".o"

@run: build
    "./"$build_dir_name"/goburin_"$in_extension

@verify:
    "./"$build_dir_name"/goburin_"$out_extension

# ---------------------------------------------

wbuild:
    watchexec -c -e asm -r -- just build 

wrun:
    watchexec -c -e asm -r -- just run

whex arg:
    watchexec -c -w {{arg}} -- hexyl {{arg}}

# ---------------------------------------------

[private]
@setup:
    mkdir -p $build_dir_name 
