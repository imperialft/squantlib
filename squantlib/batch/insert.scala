transaction {
  // one way to create an row.
  var currency = new Currency
  currency.id = "HMD"
  currency.name_jpn = "浜共和ドル"
  currency.name_jpn_short = "浜ドル"
  currency.name_eng = "Hama Republican Dollar"
  currency.name_eng_short = "HMD"
  currency.description_jpn = "テストです。"
  currency.description_eng = "This is a test."
  currency.created = Some(new java.util.Date)
  currency.lastmodified = Some(new java.util.Date)
  DB.currencies.insert(currency)

  // an another way to create a row.
  val country = new Country(
      id              = "HMA",
      currencyid      = "HMD",
      name_jpn        = "浜共和国",
      name_eng        = "The Republic of Hama",
      region          = "Interstellar",
      description_jpn = "テストです。",
      description_eng = "This is a test.",
      address_lat     = Some(0.0),
      address_lng     = Some(0.0),
      created         = Some(new java.util.Date),
      lastmodified    = Some(new java.util.Date)
    )
  DB.countries.insert(country)
}