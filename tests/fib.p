n := user;
f0 := 1;
f1 := 0;
i := 1;
while (<= i n) do {
	conditional   [ case (> i 2) do {
				cur := + f0 f1;
				f1 := f0;
				f0 := cur;
				print[cur]
			} break
			case (== i 1) do print[f1] break
			case (== i 2) do print[f0] break
		] default print[0];
	i := +i 1
}
