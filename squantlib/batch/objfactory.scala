val dbbonds:Set[Bond] = transaction { from(DB.bonds)(c => select(c)).toSet }
val fixedratebonds = dbbonds.par.map { b => b.toFixedRateBond }.seq