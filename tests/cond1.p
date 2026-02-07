z := 99;
x := 10;
y := 1;
conditional [ case (> x 0) do print [x]
	      case (> y 0) do print [y]
	    ] default print[z]
