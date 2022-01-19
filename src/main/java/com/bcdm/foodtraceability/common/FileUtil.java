package com.bcdm.foodtraceability.common;

public class FileUtil {

    //图片语序的后缀扩展名
    public static String[] IMAGE_FILE_EXTD = new String[]{"png","bmp","jpg","jpeg"};

    public static boolean isFileAllowed(String fileName){
        for (String ext: IMAGE_FILE_EXTD) {
            if (ext.equals(fileName)){
                return true;
            }
        }
        return false;
    }
}
