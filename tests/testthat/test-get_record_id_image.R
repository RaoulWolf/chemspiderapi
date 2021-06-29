library(chemspiderapi)

context("get_record_id_image")

test_that("fails if no record_id is provided.", {
  
  expect_error(
    get_record_id_image()
  )
  
})

test_that("fails if a NULL record_id is provided.", {
  
  expect_error(
    get_record_id_image(record_id = NULL)
  )
  
})

test_that("fails if a record_id is not a numeric vector.", {
  
  expect_error(
    get_record_id_image(record_id = "record_id")
  )
  
})

test_that("fails if multiple record_ids are provided.", {
  
  expect_error(
    get_record_id_image(record_id = c("123", "456"))
  )
  
})

test_that("fails if no API key is provided.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L)
  )
  
})

test_that("fails if NULL is provided as API key.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L, apikey = NULL)
  )
  
})

test_that("fails if more than one API key is provided.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L, 
                        apikey = c("API key one", "API key two"))
  )
  
})

test_that("fails if a numeric API key is provided.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L, apikey = 1234567890)
  )
  
})

test_that("fails if a logical API key is provided.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L, apikey = TRUE)
  )
  
})

test_that("fails if a non 32-character length API key is provided.", {
  
  expect_error(
    get_record_id_image(record_id = 2424L, 
                        apikey = "abcdefghijklmnopqrstuvqxyz")
  )
  
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2424/image", function(req, res) {
  res$
    set_status(200)$
    send(charToRaw(paste0("{\"image\":\"iVBORw0KGgoAAAANSUhEUgAAAJYAAACWCAYAA",
                          "AA8AXHiAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAA",
                          "JcEhZcwAADsMAAA7DAcdvqGQAAAx3SURBVHhe7Z0/iNTMG8dfQ",
                          "cXCQlBREMHmwFJQ0ELBRrHTs7IRBBtLEQs7q8NKbBSxsrhCeUH",
                          "kGu20EkEO7BRt7FQstBDxVwj7yyeb777Pjdm7272ZZLJ5PjDsZ",
                          "jaXTJJPZp6Z/Ll/Bo6TABfLSYKL5STBxXKS4GI5SXCxnCS4WE4",
                          "SXCwnCS6WkwQXy0mCi+UkwcVykuBiOUlwsZwkuFhOElwsJwkul",
                          "pMEF8tJgovlJMHFcpLgYjlJcLGcJLhYThL6LdaHD4PB5cvFXih",
                          "2g9L164PB58/VDM609FcspLJChYnfnakp9mBPmZsbCmRrKD5Vg",
                          "83PD/OcqeinWMvLQ3mQK+Tnz+FvJL47U9FvsRYWqowAait+Zz5",
                          "nKlysOjou1nJRbtLnmk6Ifgv5UMSU5P+MVEv3W6wZawqXlpaKY",
                          "v+zIs0XJ4mVRfkhzEd+nXTT0E+xQME7wboN3o8fH+Z3LHi3Uh0",
                          "vtkGiaFooL8TFioVqrXGpY8MNEubly5dVzrB5U76E0XSIixUT5",
                          "JmBAVJkQIrLbEvA4uLiClkkFnk2Uau5WBF59+7d4MWLF4M/f/5",
                          "UOYPB48ePV5z5uYMMSHH37t0qZzwSa1xysSJx5syZcoe+fv26n",
                          "P7y5Us5ffDgwXK6C0is69S2AWEvTwLR9NmkfBcrEidPnix3qGq",
                          "oT58+ldMHDhwop7uAYqm5okMSiiRxmAckUIjkcrEiMQtigcTgk",
                          "21BEGIu8kga09J0SJ1Ymrdu/rVwsWZELMShxrIyKBHAC+WFhGK",
                          "F89T9zWq4WDMiFtAMIhGSkIi5wqZNv4UsLCyU+WoyQ1ysCZkls",
                          "W7evDk4fPjwqCMSA/bFpFKBizVDYlFmyv78+fMqJx6TyuVizYh",
                          "Yb9++Lcu9Y8eOwe/fv6vc6QlFcrEmZFbEohmk3JcuXapyNg7LU",
                          "5oUF2tGxGJAl3KnaAanwcWaAbHev39fljlWMxgDF2sGxErRDG4",
                          "UF2sGxDp27FhZ5lyaQagVi0KSQhhAI38915MYaLOXFBgVXs/V9",
                          "6bpulgqb07NICQRy95gFqa6K/Bt0nWxbt26VZY3p2YQkoila1Z",
                          "cJtDVdi41aLnjLhu0QdfFyrEZhOhiqbay91kLmkaaw/DWjjaxY",
                          "t27d69zYjHMsHfv3qyaQVhVrHFpNbH4jXlyjKfq0I1++/fvLz9",
                          "37txZfuZ2ox+7kxSek//++78yP7e7qZOJRTOYO9wteurUqdF2b",
                          "d26dfT9yJEj5fhQLhRFKlN4W3vRiJT5qxySVlhVrJD1NIUSq64",
                          "pRLYcmkKaDcZ+tm3bVpaVHhVBMPn3798vmxbtg6tXrw5+/PhR/",
                          "WV7FEUZJbv7Z1YszWvnXyt45+8/fvxYHliCz5i3eazF06dPy/h",
                          "JZblw4UJZc1kQ6caNGyvEu3PnTvVrOxTFGKVi946axJkUK5xH0",
                          "6q16pIeUfr69euKmqHuAMeEq/8K1EnrEZpAXjEYibirrd5Xsfo",
                          "ysfv4VAg7szWWxf7NegZI62oGNUmxYB1XrlwZlQOZae4mAZl0k",
                          "ZeEbE3HX8Vqy0SQru+M2nRKrEnRDp+WsGagqaLJ2ig0X8iq5SL",
                          "xRuKlcHmp4y+GQIgFHz16VKxvKBBwfvIdqWZaLMHO3ghhzUDTR",
                          "RM2KSlrGERCKC07dvzFCcUoug0Thtvyn1hA30h5pJkSi422hNP",
                          "TEtYMw6ZsuAOrAfIR9oz9/v37XzVfqpgIUW3MNm38RbMvmew2a",
                          "5nUWMOHaIfbKdhe5ZFmrsayOyImYc3A4pXsaIUV68mTJ+W8HKD",
                          "Ysdo4wl4mYtO0rwblonmjw6L4UunQoUPliRUuo/ipTBaGCpXP9",
                          "n/79q36pX2CouYHNQM7WjuQZMderVi/fv0q503Zu6wDURBZNQ6",
                          "yhPEc3x8+fDg4d+7cXzJR89XJZCGuCvo+5QmmfIL606dPN77t4",
                          "8heLIE8Nqnqzyl45aDaHuiePXtKkUhWJBIy0TuNIQJi62I068q",
                          "B4pB0g2KflWlpafipgf0ce0V0OHSgbeKgU2ul6ElS26nGpPZrm",
                          "86JBZKJJiBHsQQPj3KgL168mESmEOI91kdTO01vOiadFIuBQU1",
                          "zeYPPHMWip8eBppZqCq2THmUTnZdxVIcqfySSoLZSHqkrYvE9x",
                          "uDvOJBJY3isvy3MocobCSToEam2InVBrOF4VPqbCOlJq+fZZG1",
                          "p6axYgEzK74JYBNhNiAX0OFkXAf1a42op6IxYIXTXb9++XU3lS",
                          "ZtigYY56KE2HW91UqymD9C0tC0WPVHWxTq5itEkLlZC2hYLuOe",
                          "MdZL0JFITuFgJyUEs0CP43DHR1CUfFyshuYgFuhODzyZwsRKSk",
                          "1jUVLrH69q1a1VuOlyshLQpFj1ChhrspaQHDx6U69+0aVOVkw4",
                          "XKyFtiWVjKg0z2Dsgjh49WualxMVKSBtijesFcn8YecjWxAVxF",
                          "yshTYs1btwKwcgjNTXk4GIlpGmx6kbabdBOE9kULlZCmhSLdbB",
                          "sLj6zHqGHS5oaZhAuVkKaEmvc3QzDZwXauRDtYiWkCbFo8niyh",
                          "+WyPkEQL9lS3v81DhcrIU2IpUfkWKZ6e8jGNPlNX3wWnRRr+Mh",
                          "598QCYh6e5ImBvcfdvuBE623z9uROiUVXWdV+V8WKhe3t8UyjY",
                          "F3kIVus1wpMQyfEYieqK03avXt3+blv375qjjw5f/58WU7ebRo",
                          "b7Q/b28vhlmSRtVhU44y9aGfxybR93TfNSlO3gqwXyk0tsnnz5",
                          "lE5ESHWA6rA8mjqtDzWqUs2PLrfNtmKRRylql4Hxh6Us2fPjn6",
                          "jO53DQ5oQvulm+/bto+9K1DIxJYO6IL5NshOLINQ+RUxMFV6G4",
                          "IBwVvL7li1bRvNyQFO9XWYtaIbsm25sWTjQk74EZBJY/okTJwa",
                          "7du1aEcS3STZiIYuCXRK1VfjmvbqmkSYnrCXW88aXWHBQVVuQ1",
                          "qo92QZJxrz6OxKSsT11QTdPfJN4WNfCC1KU/+zZsyq3fVoXS/G",
                          "IdjKy1L15L3xdEE2jlWe9y4kJ4ls5EGzS9bFdnFChZJwonESSr",
                          "MgqU/gy6lxfMdCqWKEs1DTh2Rq+lLauabRQ84XvHI3dQwqHPer",
                          "KPQ3sD8puY0stv/gYpcXF6g8KXCwDtYuVpS42qhMkbBpXY5q3J",
                          "K8FNaS6+SROCmRIAfJKsuE6/xOLpP9E0V+x2GKSCQ44wBwYqv+",
                          "6eCRmkxbWisQ26o2paCEUlXy9OZATgTIotqNslJH8pihWWya9s",
                          "0L/oaJ/YulFVjbxsoWqGaNGCWUJg/AwjpoWBLBBP2KAisXBsti",
                          "DRc1hm6a2xs1UVvvOCnZlv8SyUrEX2Hr7Bo9KLhF21deKo6YFI",
                          "TRMASoOyf6TI3uwFEvRrLb5zimVE9g1fGeX6u3Jsy8Wp5T2AoJ",
                          "ZVI+zRyp0zxBp0jhqWsKeFgmZhBWLGjOHsSGVU+g/VCjNvlhsI",
                          "Vta80+aSlRzVVUETV8TQwN16KDorNd5kGPzorIK+x8qcisrmKJ",
                          "GQmKFgYsIjhrNU1ODmSH2oOg7FW4XxAKGHZTfH7HG/e/njI6aD",
                          "gpQXBU7R7EoS115lK8ebC5UuzUi9NV1xMKttb9lgC2KDQ2VchK",
                          "ra1S7NTI65YmnCFw4QranmMl/sldxhC0iycWaHrNbI0JkqYg4T",
                          "ORnUm+rSBadEyQXa3qC3RoR5CG6pF/M0eIzHH5oGYpFsnBOKN9",
                          "cLHAmJJ1YHYPBT8bRXr16VeU4G8HFquCSD4O0fDobx8WqcLHi4",
                          "mJVuFhxcbEqXKy4uFgVLlZcXKwKFysuLlaFixUXF6vCxYqLi1X",
                          "hYsXFxapwseLiYlW4WHFxsSp4TAyxmrjnvg+4WBXLy8uDN2/eD",
                          "D7bx3Uq+I0UwuuUyP+Z2+2bGdB7sZaWlsqayqb5+fkVsig/hPn",
                          "Ir5Ou7/RaLCvV8ePHR6JoWigvxMUaT6/FkjD24Vj7tkAJo+kQF",
                          "2s8vRULGZDisl6CYFhcXFwhi8QizyZqNRernt6LdXfc848GiTU",
                          "uuVh/03uxrtc8MRT28iQQTZ9NLtZ4eiuWYqm5ubm/RJI4zAMSK",
                          "ERyuVh/01uxQGLwSQCPIMRcEkljWpoOqRNL89bN3yd6vfWIQ41",
                          "lZVAigBfKCwnFCuep+5u+0O/TqoBmEImQhETMFTZt+i1kYWGhz",
                          "FeTGeJiOVFBqD5LBS5WQrzGcqIQiuRiOdFQM9hnqcDFcpLgYjl",
                          "JcLGcJLhYThJcLCcJLpaTBBfLSYKL5STBxXKS4GI5SXCxnCS4W",
                          "E4SXCwnCS6WkwQXy0mCi+UkwcVyEjAY/B9t7oqZhT1auwAAAAB",
                          "JRU5ErkJggg==\"}")))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_RECORD_ID_URL" = web$url())

test_that("returns a proper response.", {
  
  expect_type(
    get_record_id_image(record_id = 2424L,
                       apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
  
})

Sys.unsetenv("GET_RECORD_ID_URL")

web$stop()
