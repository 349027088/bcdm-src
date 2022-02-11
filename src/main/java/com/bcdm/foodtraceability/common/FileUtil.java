package com.bcdm.foodtraceability.common;

public class FileUtil {

    //图片语序的后缀扩展名
    public static String[] IMAGE_FILE_EXTD = new String[]{"png", "bmp", "jpg", "jpeg"};

    public static boolean isFileAllowed(String fileName) {
        for (String ext : IMAGE_FILE_EXTD) {
            if (ext.equals(fileName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 图片大小判断
     *
     * @param len 图片实际大小
     * @param size 图片大小
     * @param unit 图片大小单位
     * @return true 图片符合标准 false 图片不符合标准
     */
    public static boolean isFileSizeOver(Long len, int size, String unit) {
        double fileSize = 0;
        if ("B".equalsIgnoreCase(unit)) {
            fileSize = (double) len;
        } else if ("K".equalsIgnoreCase(unit)) {
            fileSize = (double) len / 1024;
        } else if ("M".equalsIgnoreCase(unit)) {
            fileSize = (double) len / 1048576;
        } else if ("G".equalsIgnoreCase(unit)) {
            fileSize = (double) len / 1073741824;
        }
        if (fileSize > size){
            return false;
        }
        return true;
    }
}
