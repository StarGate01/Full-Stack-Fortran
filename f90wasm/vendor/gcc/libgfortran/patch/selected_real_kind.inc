integer, parameter :: c = 4
type (real_info), parameter :: real_infos(c) = (/ &
    real_info (4, precision(0.0_4), range(0.0_4), radix(0.0_4)), &
    real_info (8, precision(0.0_8), range(0.0_8), radix(0.0_8)), &
    real_info (10, precision(0.0_10), range(0.0_10), radix(0.0_10)), &
    real_info (16, precision(0.0_16), range(0.0_16), radix(0.0_16)) /)