set export
set shell := ["fish", "-c"]

# dir names
src_dir := "src"
build_dir := "build"
tests_dir := "tests"

# extension names
in_ext := "asm"
out_ext := "forth"

@build: setup
    fasm $src_dir"/goburin_"$in_ext"."$in_ext $build_dir"/goburin_"$in_ext".o"
    ld -o $build_dir"/goburin_"$in_ext $build_dir"/goburin_"$in_ext".o"
    rm -f -- $build_dir"/goburin_"$in_ext".o"

@run: build
    "./"$build_dir"/goburin_"$in_ext  < $src_dir"/goburin_"$out_ext"."$out_ext

@verify: run
    "./"$build_dir"/goburin_"$out_ext

@test: build
    for test_file in $tests_dir"/goburin_"$out_ext"/"*"."$out_ext; \
        echo "Running test [ $test_file ]" ; \
        "./$build_dir/goburin_$in_ext"  < $test_file ; \
        "./$build_dir/goburin_$out_ext" ; \
        set status_code $status ; \
        set base_name (basename $test_file .$out_ext) ; \
        set expected_file "$tests_dir/goburin_$out_ext/$base_name.out" ; \
        set expected_status (cat $expected_file) ; \
        if test "$status_code" = "$expected_status" ; \
            echo "--> Success" ; \
        else ; \
            echo "--> Failure" ; \
        end ; \
    end 

# ---------------------------------------------

wbuild:
    watchexec -c -e asm -r -- just build 

wrun:
    watchexec -c -e asm -r -- just run

wverify:
    watchexec -c -e asm -r -- just verify

wtest:
    watchexec -c -r -- just test

whex arg:
    watchexec -c -w {{arg}} -- hexyl {{arg}}
    

# ---------------------------------------------

[private]
@setup:
    mkdir -p $build_dir 
