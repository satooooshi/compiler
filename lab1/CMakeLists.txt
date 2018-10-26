cmake_minimum_required(VERSION 3.12)
project(lab1)

set(CMAKE_CXX_STANDARD 14)

include_directories(.)

add_executable(lab1
        gradeMe.sh
        main.c
        myimpl.c
        prog1.c
        prog1.h
        ref-0.txt
        ref-1.txt
        ref.txt
        slp.c
        slp.h
        util.c
        util.h)
