integer, parameter :: c = 2
type (real_info), parameter :: real_infos(c) = (/ &
    real_info (4, precision(0.0_4), range(0.0_4), radix(0.0_4)), &
    real_info (8, precision(0.0_8), range(0.0_8), radix(0.0_8)) /)