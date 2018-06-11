is_max_cyclic_sub := function(sub, g)
	local subs, flag,i;

	subs := Subgroups(g);

	subs := Filtered(subs, IsCyclic);

	if IsCyclic(sub) then
		flag := true;
	else
		flag := false;
	fi;		
	
	for i in [1 .. Length(subs)] do
		if (Size(Intersection(subs[i], sub)) > 1) and 
			(Size(Union(subs[i], sub)) > Size(sub)) then
			flag := false;
		fi ;
	od ;
	
	return flag;
end;


pnumber_mcs := function(g)
# % pnumber computes the partition number of g

	local subs,u,pnumber,i;

	# % first we gather all subgroups of g
	subs := Subgroups(g);

	# % filter out any that are non max cyclic subgroups
	subs := Filtered(subs, x -> is_max_cyclic_sub(x, g));

	# % filter out with trivial subgroups
	subs := Filtered(subs, x -> is_nontrivial(x, g));


	# % then all possible combinations
	u := Combinations(subs);

	# % filter out any that are not a partition
	u := Filtered(u, x -> is_partition(x, g));
	
	# % then check the sizes of our partitions
	for i in [1 .. Length(u)] do
		u[i] := Length(u[i]);
	od;
	
	# % if we have any partitions, pick the smallest
	# % otherwise return 0 as the pnumber
	if Length(u) <> 0 then
		pnumber := Minimum(u);
	else
		pnumber := 0;
	fi ;
	
	return pnumber;
end;


