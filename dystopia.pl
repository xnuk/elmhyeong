# Don't try this at home
if ($ENV{'CIRCLECI'}) {
	foreach(grep {$_ !~ /^(\.+(git)?|dist)$/} <.* *>) { `rm -rf $_` }
}
