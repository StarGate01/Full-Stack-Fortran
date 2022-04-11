integer, parameter :: c = 4
type (int_info), parameter :: int_infos(c) = (/ &
    int_info (1, range(0_1)), &
    int_info (2, range(0_2)), &
    int_info (4, range(0_4)), &
    int_info (8, range(0_8)) /)