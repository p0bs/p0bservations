test_that("error checks work", {
  
  expect_error(
    get_price_Zoopla(
      link_house = TRUE
    ), 
    regexp = "link_house must be a character input"
  )
  
  expect_error(
    get_price_Zoopla(
      link_house = c(
        'https://www.zoopla.co.uk/property/10-waverleigh-road/cranleigh/gu6-8bz/9852441', 
        'https://www.zoopla.co.uk/property/flat-3/queens-square-court-247-248/east-road/tylorstown/ferndale/cf43-3hg/4291721'
        )
    ), 
    regexp = "You must enter one, and only one, entry for link_house"
  )
  
  expect_error(
    get_price_Zoopla(
      link_house = 'https://www.zoopla.co.uk/property'
    ), 
    regexp = "link_house must begin with 'https://www.zoopla.co.uk/property/'"
  )
  
})

test_that("calculations work", {
  
  expect_true(
    is.numeric(
      get_price_Zoopla(
        link_house = 'https://www.zoopla.co.uk/property/10-waverleigh-road/cranleigh/gu6-8bz/9852441'
      )
    )
  )
  
  expect_lte(
    get_price_Zoopla(
      link_house = 'https://www.zoopla.co.uk/property/flat-3/queens-square-court-247-248/east-road/tylorstown/ferndale/cf43-3hg/4291721'
    ),
    200000
  )
  
  expect_gte(
    get_price_Zoopla(
      link_house = 'https://www.zoopla.co.uk/property/1-oak-road/reigate/rh2-0bp/19534314'
    ),
    800000
  )
  
  expect_gte(
    get_price_Zoopla(
      link_house = 'https://www.zoopla.co.uk/property/10-waverleigh-road/cranleigh/gu6-8bz/9852441'
    ),
    300000
  )
  
})