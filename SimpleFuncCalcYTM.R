
#last_price_dirty最新价格（全价）
#last_price_clean最新价格（净价）
#last_ytm:YIELD
#dur:久期
#price_clean所求的净价

last_price_dirty=96.5714
last_price_clean=95.9647
last_ytm=4.15
dur=6.1482
price_clean=95.9362

func_calculate_ytm = function(last_price_dirty,last_price_clean,last_ytm,dur,price_clean)
{
  ytm_diff = -(price_clean - last_price_clean)/dur/last_price_dirty*100
  ytm_diff = round(ytm_diff/0.005)*0.005
  ytm = last_ytm + ytm_diff
  ytm
}

