file( GLOB FFTPACK_FILES ${SOCORRO_SRC_DIR}/*.c )
add_library( fftpack ${FFTPACK_FILES} )
target_link_libraries( ${SOCORRO_EXE} fftpack )
