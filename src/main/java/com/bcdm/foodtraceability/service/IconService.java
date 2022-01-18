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
    void deleteIcon(String URI) throws Exception;

    /**
     * 图片验证
     * @param icon 图片
     * @return 图片是否符合标准
     * @throws Exception 图片异常
     */
    boolean IconCheck(MultipartFile icon) throws Exception;

    /**
     * 创建图片上传至七牛云
     *
     * @param icon 需要被上传的图片
     * @return 返回图片被上传的地址
     * @throws Exception 图片上传失败
     */
    String sendIconToCloud(MultipartFile icon) throws Exception;
}
