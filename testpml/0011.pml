never{
    atomic{
        if
        :: a == 2 -> printm(a)
        :: a == 3 -> break
        fi
    }
}