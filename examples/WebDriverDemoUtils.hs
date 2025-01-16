module WebDriverDemoUtils where

import WebDriverSpec (Selector (CSS))


-- ################### Effectful Demo ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

checkBoxesLinkCss :: Selector
checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

topFrameCSS :: Selector
topFrameCSS = CSS "frame[name='frame-top']"

midFrameCss :: Selector
midFrameCss = CSS "frame[name='frame-middle']"

bottomFrameCss :: Selector
bottomFrameCss = CSS "frame[name='frame-bottom']"

midFrameTitle :: Selector
midFrameTitle = CSS "#content"

framesUrl :: Text
framesUrl = "https://the-internet.herokuapp.com/nested_frames"
