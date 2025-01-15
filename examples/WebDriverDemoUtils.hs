module WebDriverDemoUtils where

import WebDriverSpec (Selector (CSS))


-- ################### Effectful Demo ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

checkBoxesLinkCss :: Selector
checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"
