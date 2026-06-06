e <- 1.602176634e-19 # elementary charge (C)
R <- 3e11            # representative resistor (Ohm)

# intensity_unit_info() =======================================================

test_that("intensity_unit_info()", {
  # voltage units ==============================================================
  expect_equal(intensity_unit_info("V"),  list(base = "V", scale = 1))
  expect_equal(intensity_unit_info("mV"), list(base = "V", scale = 1e3))

  # current units ==============================================================
  expect_equal(intensity_unit_info("A"),  list(base = "A", scale = 1))
  expect_equal(intensity_unit_info("mA"), list(base = "A", scale = 1e3))
  expect_equal(intensity_unit_info("µA"), list(base = "A", scale = 1e6))
  expect_equal(intensity_unit_info("nA"), list(base = "A", scale = 1e9))
  expect_equal(intensity_unit_info("pA"), list(base = "A", scale = 1e12))
  expect_equal(intensity_unit_info("fA"), list(base = "A", scale = 1e15))

  # cps ========================================================================
  expect_equal(intensity_unit_info("cps"), list(base = "cps", scale = 1))
})

# ir_convert_intensity() ======================================================

test_that("ir_convert_intensity()", {
  r <- tibble::tibble(species = "CO2", channel = 1L, resistance.Ohm = R)
  d_V   <- tibble::tibble(species = "CO2", channel = 1L, intensity.V   = 3.0)
  d_mV  <- tibble::tibble(species = "CO2", channel = 1L, intensity.mV  = 3000.0)
  d_A   <- tibble::tibble(species = "CO2", channel = 1L, intensity.A   = 1e-11)
  d_nA  <- tibble::tibble(species = "CO2", channel = 1L, intensity.nA  = 10.0)
  d_cps <- tibble::tibble(species = "CO2", channel = 1L, intensity.cps = 1e6)

  # argument errors ============================================================
  expect_error(ir_convert_intensity(list()), "data frame")
  expect_error(ir_convert_intensity(tibble::tibble(species = "CO2")), "channel")
  expect_error(
    ir_convert_intensity(tibble::tibble(species = "CO2", channel = 1L)),
    "intensity"
  )
  expect_error(ir_convert_intensity(d_V, units = "xxx"), "units")
  # resistors required for V <-> A/cps conversions
  expect_error(ir_convert_intensity(d_V,   units = "nA"),  "resistors")
  expect_error(ir_convert_intensity(d_V,   units = "cps"), "resistors")
  expect_error(ir_convert_intensity(d_mV,  units = "pA"),  "resistors")
  expect_error(ir_convert_intensity(d_A,   units = "mV"),  "resistors")
  expect_error(ir_convert_intensity(d_cps, units = "V"),   "resistors")

  # output column and source column replacement ================================
  out <- ir_convert_intensity(d_V, units = "mV")
  expect_true("intensity.mV" %in% names(out))
  expect_false("intensity.V" %in% names(out))
  expect_false("resistance.Ohm" %in% names(out))

  out_r <- ir_convert_intensity(d_V, r, units = "nA")
  expect_true("intensity.nA" %in% names(out_r))
  expect_false("intensity.V" %in% names(out_r))
  expect_false("resistance.Ohm" %in% names(out_r))

  # voltage <-> voltage (no resistors needed) ==================================
  expect_equal(ir_convert_intensity(d_V,  units = "V" )$intensity.V,  3.0)
  expect_equal(ir_convert_intensity(d_V,  units = "mV")$intensity.mV, 3000.0)
  expect_equal(ir_convert_intensity(d_mV, units = "V" )$intensity.V,  3.0)
  expect_equal(ir_convert_intensity(d_mV, units = "mV")$intensity.mV, 3000.0)

  # current <-> current (no resistors needed) ==================================
  # d_A = 1e-11 A = 10 pA = 0.01 nA = 10000 fA; d_nA = 10 nA = 1e-8 A = 1e7 fA
  expect_equal(ir_convert_intensity(d_A,  units = "A" )$intensity.A,  1e-11)
  expect_equal(ir_convert_intensity(d_A,  units = "nA")$intensity.nA, 1e-11 * 1e9)
  expect_equal(ir_convert_intensity(d_A,  units = "pA")$intensity.pA, 1e-11 * 1e12)
  expect_equal(ir_convert_intensity(d_A,  units = "fA")$intensity.fA, 1e-11 * 1e15)
  expect_equal(ir_convert_intensity(d_nA, units = "nA")$intensity.nA, 10.0)
  expect_equal(ir_convert_intensity(d_nA, units = "A" )$intensity.A,  10 * 1e-9)
  expect_equal(ir_convert_intensity(d_nA, units = "fA")$intensity.fA, 10 * 1e-9 * 1e15)

  # current <-> cps (no resistors needed) =====================================
  expect_equal(ir_convert_intensity(d_A,   units = "cps")$intensity.cps, 1e-11 / e)
  expect_equal(ir_convert_intensity(d_cps, units = "A"  )$intensity.A,   1e6 * e)
  expect_equal(ir_convert_intensity(d_cps, units = "fA" )$intensity.fA,  1e6 * e * 1e15)

  # voltage <-> current (resistors required) ===================================
  expect_equal(ir_convert_intensity(d_V, r, units = "A"  )$intensity.A,   3.0 / R)
  expect_equal(ir_convert_intensity(d_V, r, units = "nA" )$intensity.nA,  3.0 / R * 1e9)
  expect_equal(ir_convert_intensity(d_V, r, units = "fA" )$intensity.fA,  3.0 / R * 1e15)
  expect_equal(ir_convert_intensity(d_A, r, units = "V"  )$intensity.V,   1e-11 * R)
  expect_equal(ir_convert_intensity(d_A, r, units = "mV" )$intensity.mV,  1e-11 * R * 1e3)
  expect_equal(ir_convert_intensity(d_mV, r, units = "pA")$intensity.pA,  3.0 / R * 1e12)

  # voltage <-> cps (resistors required) ======================================
  expect_equal(ir_convert_intensity(d_V,   r, units = "cps")$intensity.cps, 3.0 / R / e)
  expect_equal(ir_convert_intensity(d_cps, r, units = "V"  )$intensity.V,   1e6 * e * R)
  expect_equal(ir_convert_intensity(d_cps, r, units = "mV" )$intensity.mV,  1e6 * e * R * 1e3)

  # config join key ============================================================
  r2 <- tibble::tibble(
    species = "CO2", channel = 1L,
    config = c(1L, 2L),
    resistance.Ohm = c(1e11, R)
  )
  d_cfg1 <- tibble::tibble(species = "CO2", channel = 1L, config = 1L, intensity.A = 1e-11)
  d_cfg2 <- tibble::tibble(species = "CO2", channel = 1L, config = 2L, intensity.A = 1e-11)

  expect_equal(
    ir_convert_intensity(d_cfg1, r2, units = "V")$intensity.V,
    1e-11 * 1e11
  )
  expect_equal(
    ir_convert_intensity(d_cfg2, r2, units = "V")$intensity.V,
    1e-11 * R
  )

  # config ignored when absent from resistors ==================================
  r_no_cfg <- tibble::tibble(species = "CO2", channel = 1L, resistance.Ohm = R)
  expect_equal(
    ir_convert_intensity(d_cfg1, r_no_cfg, units = "V")$intensity.V,
    1e-11 * R
  )
})
