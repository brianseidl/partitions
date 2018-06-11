LoadPackage("sonata");

is_partition := function(subs, g)
	# % is_partition takes a collection of subgroups subs, and checks
	# % if they are a partition for a group g. it returns true if
	# % they are, and false if they are not.

	local u, flag, i;

	flag := true;

	# % first find all pairs of subgroups
	u := Combinations(subs,2);
	
	# % if the union is not all of g, then we can’t have a partition
	if Size(Union(subs)) <> Size(g) then
		flag := false;
	fi;
	
	# % or if any pairwise intersection is greater than 1, no partition
	if flag = true then
		for i in [1 .. Length(u)] do
			if Size(Intersection(u[i])) <> 1 then
				flag := false;
			fi;
		od;
	fi;
	
	return flag;
end;

is_cover := function(subs, g)
	# % is_partition takes a collection of subgroups subs, and checks
	# % if they are a partition for a group g. it returns true if
	# % they are, and false if they are not.

	local u, flag, i;

	flag := true;
	
	# % if the union is not all of g, then we can’t have a partition
	if Size(Union(subs)) <> Size(g) then
		flag := false;
	fi;
	
	return flag;
end;


is_nontrivial := function(sub, g)
	# % is_nontrivial_subs checks if any of the subgroups in subs is trivial,
	# % meaning either the identity or all of g
	
	local flag;
	
	flag := true;
	if Size(sub) = 1 then
		flag := false;
	elif Size(sub) = Size(g) then
		flag := false;
	fi ;
	
	return flag;
end;


pnumber := function(g)
# % pnumber computes the partition number of g

	local subs,u,pnumber,i;

	# % first we gather all subgroups of g
	subs := Subgroups(g);

	# % filter out  trivial subgroups
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

cnumber := function(g)
# % pnumber computes the cover number of g

	local subs,u,cnumber,i;

	# % first we gather all subgroups of g
	subs := Subgroups(g);

	# % filter out any combinations with trivial subgroups
	subs := Filtered(subs, x -> is_nontrivial(x, g));

	# % then all possible combinations
	u := Combinations(subs);

	# % filter out any that are not a cover
	u := Filtered(u, x -> is_cover(x, g));
		
	# % then check the sizes of our partitions
	for i in [1 .. Length(u)] do
		u[i] := Length(u[i]);
	od;
	
	# % if we have any partitions, pick the smallest
	# % otherwise return 0 as the pnumber
	if Length(u) <> 0 then
		cnumber := Minimum(u);
	else
		cnumber := 0;
	fi ;
	
	return cnumber;
end;

is_max_cyclic_sub := function(sub, g)
	local subs, flag, i;

	subs := Subgroups(g);

	subs := Filtered(subs, IsCyclic);

	if IsCyclic(sub) then
		flag := true;
	else 
		flag := false; fi;
	
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

pnumber_max := function(g, max)
# % pnumber computes the partition number of g

	local subs,u,pnumber,i;

	# % first we gather all subgroups of g
	subs := Subgroups(g);

	# % filter out  trivial subgroups
	subs := Filtered(subs, x -> is_nontrivial(x, g));

	# % then all possible combinations
	u := Combinations(subs);

	# % Filter out all combinations which size > max
	u := Filtered(u, x -> Size(x) <= max);
	
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

Dih := function(N)
# % takes the general dihedral group of N
# % idk how it work but it does work lol

	local au, apc, G, apci, p;
	au := AutomorphismGroup(CyclicGroup(2));
	apc := IsomorphismPcGroup(au);
	G := Image(apc);
	apci := InverseGeneralMapping(apc);
	p := SemidirectProduct(G,apci,N);
	return p;

end;
