never{
    if
    ::  vm_state == vm_waiting && event == input_money ->
        vm_state = vm_trading
    ::  vm_state == vm_trading && event == push_buybtn ->
        vm_state == vm_buying
    ::  vm_state == vm_calculating_change && event == 		push_changebtn ->
        vm_state = vm_waiting
    ::  else -> true
    fi
}