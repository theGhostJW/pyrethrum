{-# LANGUAGE CPP #-}

module DemoProject.Test.TestFilePaths where

import  Foundation.Extended

invalidFile :: Path Abs File  
invalidFile = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\Blahhh.txt|] -- windows
#else 
  [absfile|/mnt/c/vids/systemdesign/blahhh.txt|] -- linux
#endif

invalidFile2 :: Path Abs File  
invalidFile2 = 
#ifdef mingw32_HOST_OS 
  [absfile|R:\Vids\SystemDesign\Wrong.txt|] -- windows
#else 
  [absfile|/mnt/r/Vids/SystemDesign/Wrong.txt|] -- linux
#endif

validFile :: Path Abs File  
validFile = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\VidList.txt|] -- windows
#else 
  [absfile|/mnt/c/Vids/SystemDesign/VidList.txt|] -- linux
#endif

validFileWithSpace :: Path Abs File  
validFileWithSpace = 
#ifdef mingw32_HOST_OS 
  [absfile|C:\Vids\SystemDesign\Vid List.txt|] -- windows
#else 
  [absfile|/mnt/c/Vids/SystemDesign/Vid List.txt|] -- linux
#endif