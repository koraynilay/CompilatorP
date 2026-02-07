z := 99;
x := 10;
y := 1;
conditional [ case (> x 20) do print [x] break
              case (> y 0) do print [y] break
	    ] default print[z]
