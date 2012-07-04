val dbbonds = transaction { from(DB.bonds)(c => select(c)).toSet }
val fixedratebonds = FixedRateBondConstructor.getbonds(dbbonds)