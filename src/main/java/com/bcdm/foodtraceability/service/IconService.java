package com.bcdm.foodtraceability.service;

import org.springframework.web.multipart.MultipartFile;

public interface IconService {

    /**
     * 创建图片
     *
     * @param icon 需要被创建的图片
     * @return 返回图片被创造的地址
     * @throws Exception 图片创造失败
     */
    String createIcon(MultipartFile icon) throws Exception;

    /**
     * 删除图片
     * @param URI 需要删除的图片地址
     * @throws Exception 删除图片失败
     */
    boolean deleteIcon(String URI) throws Exception;
}
