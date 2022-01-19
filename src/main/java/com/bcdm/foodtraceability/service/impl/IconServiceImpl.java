package com.bcdm.foodtraceability.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.common.CreateUUID;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.service.IconService;
import com.qiniu.common.QiniuException;
import com.qiniu.http.Response;
import com.qiniu.storage.Configuration;
import com.qiniu.storage.Region;
import com.qiniu.storage.UploadManager;
import com.qiniu.util.Auth;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.ICON_TYPE_FORMAT_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.ICON_UPLOAD_FAIL;

@Service
public class IconServiceImpl implements IconService {

    @Value("${foodTraceability.accessKey}")
    private String ACCESS_KEY;

    @Value("${foodTraceability.secretKey}")
    private String SECRET_KEY;

    @Value("${foodTraceability.bucket}")
    private String bucketName;

    @Value("${foodTraceability.bucket}")
    private String iconServiceLink;

    private final Configuration cfg = new Configuration(Region.region0());

    private final UploadManager uploadManager = new UploadManager(cfg);

    @Override
    public String createIcon(MultipartFile icon) throws Exception {
        return sendIconToCloud(icon);
    }

    @Override
    public void deleteIcon(String URI) throws Exception {
        //TODO
    }


    /**
     * 发送图片至千牛云地址
     * @param icon 需要发送的图片
     * @return 返回上传的地址
     * @throws Exception 图片发送失败
     */
    private String sendIconToCloud(MultipartFile icon) throws Exception {
        int dotPos = icon.getOriginalFilename().lastIndexOf(CUT_POINT);
        if (dotPos < 0) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_TYPE_FORMAT_FAIL);
        }
        String fileExt = icon.getOriginalFilename().substring(dotPos + 1).toLowerCase();
        String fileName = CreateUUID.getUUID() + CUT_POINT + fileExt;
        try {
            Response res = uploadManager.put(icon.getBytes(), fileName, Auth.create(ACCESS_KEY, SECRET_KEY).uploadToken(bucketName));
            if (res.isOK() && res.isJson()) {
                return iconServiceLink + JSONObject.parseObject(res.bodyString()).get("key");
            }
        } catch (QiniuException e) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, e.getMessage());
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_UPLOAD_FAIL);
    }

}
