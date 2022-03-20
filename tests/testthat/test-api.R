context("API wrappers")

httptest::without_internet({
    test_that("HTTP verb functions make requests", {
      httptest::expect_GET(sc_GET("http://httpbin.org/"))
      httptest::expect_PUT(sc_PUT("http://httpbin.org/"))
      httptest::expect_POST(sc_POST("http://httpbin.org/"))
        #httptest::expect_PATCH(sc_PATCH("http://httpbin.org/"))
      httptest::expect_DELETE(sc_DELETE("http://httpbin.org/"))
    })
    test_that("Our user-agent string is set", {
      httptest::expect_GET(
        httptest::expect_header(
                sc_GET("http://httpbin.org/"),
                "user-agent: shortcutr/0.1.0"
            )
        )
    })
})

httptest::with_mock_api({
  testthat::test_that("Successful requests are handled", {
    testthat::expect_identical(
            sc_GET("https://example.com/get"),
            list(success="Yes!")
        )
    })

  testthat::test_that("Bad requests return an error", {
    testthat::expect_error(sc_GET("https://example.com/404"),
            "Not Found")
    })
})

