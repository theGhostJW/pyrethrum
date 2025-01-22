module WebDriverDemoUtils where

import WebDriverSpec (Selector (CSS))


-- ################### Effectful Demo ##################

theInternet :: Text
theInternet = "https://the-internet.herokuapp.com/"

checkBoxesLinkCss :: Selector
checkBoxesLinkCss = CSS "#content > ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"

checkBoxesCss :: Selector
checkBoxesCss = CSS "input[type='checkbox']"

topFrameCSS :: Selector
topFrameCSS = CSS "frame[name='frame-top']"

midFrameCss :: Selector
midFrameCss = CSS "frame[name='frame-middle']"

bottomFrameCss :: Selector
bottomFrameCss = CSS "frame[name='frame-bottom']"

divCss :: Selector
divCss = CSS "div"

midFrameTitle :: Selector
midFrameTitle = CSS "#content"

userNameCss :: Selector
userNameCss = CSS "#username"

framesUrl :: Text
framesUrl = "https://the-internet.herokuapp.com/nested_frames"

inputsUrl :: Text
inputsUrl = "https://the-internet.herokuapp.com/inputs"

loginUrl :: Text
loginUrl = "https://the-internet.herokuapp.com/login"

checkBoxesUrl :: Text
checkBoxesUrl = "https://the-internet.herokuapp.com/checkboxes"

shadowDomUrl :: Text
shadowDomUrl = "https://the-internet.herokuapp.com/shadowdom"

contentCss :: Selector
contentCss = CSS "#content"

inputTagCss :: Selector
inputTagCss = CSS "input"

h3TagCss :: Selector
h3TagCss = CSS "h3"

anyElmCss :: Selector
anyElmCss = CSS "*"



