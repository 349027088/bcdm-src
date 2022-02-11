package com.bcdm.foodtraceability.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.common.CreateUUID;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.service.IconService;
import com.qiniu.common.QiniuException;
import com.qiniu.http.Response;
import com.qiniu.storage.*;
import com.qiniu.util.Auth;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.Objects;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.FileUtil.isFileAllowed;
import static com.bcdm.foodtraceability.common.FileUtil.isFileSizeOver;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 图片功能服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class IconServiceImpl implements IconService {

    @Value("${foodTraceability.accessKey}")
    private String ACCESS_KEY;

    @Value("${foodTraceability.secretKey}")
    private String SECRET_KEY;

    @Value("${foodTraceability.bucket}")
    private String bucketName;

    private final Configuration cfg = new Configuration(Region.region0());

    private final UploadManager uploadManager = new UploadManager(cfg);

    @Override
    public String createIcon(MultipartFile icon) throws Exception {
        return sendIconToCloud(icon);
    }

    @Override
    public void deleteIcon(String URI) throws Exception {
        deleteIconCloud(URI);
    }


    /**
     * 发送图片至千牛云地址
     *
     * @param icon 需要发送的图片
     * @return 返回上传的子链接地址
     * @throws Exception 图片发送失败
     */
    private String sendIconToCloud(MultipartFile icon) throws Exception {
        int dotPos = Objects.requireNonNull(icon.getOriginalFilename()).lastIndexOf(CUT_POINT);
        if (dotPos < 0) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_TYPE_FORMAT_FAIL);
        }
        String fileExt = icon.getOriginalFilename().substring(dotPos + 1).toLowerCase();
        if (!isFileAllowed(fileExt)) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_TYPE_FORMAT_FAIL);
        }
        if (!isFileSizeOver(icon.getSize(),2,"M")){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_SIZE_FAIL);
        }
        String fileName = CreateUUID.getUUID() + CUT_POINT + fileExt;
        try {
            Response res = uploadManager.put(icon.getBytes(), fileName, Auth.create(ACCESS_KEY, SECRET_KEY).uploadToken(bucketName));
            if (res.isOK() && res.isJson()) {
                return (String) JSONObject.parseObject(res.bodyString()).get("key");
            }
        } catch (QiniuException e) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, e.getMessage());
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_UPLOAD_FAIL);
    }

    private void deleteIconCloud(String URI) throws Exception {
        try {
            new BucketManager(Auth.create(ACCESS_KEY, SECRET_KEY), cfg).delete(bucketName, URI);
        } catch (QiniuException e) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, e.getMessage());
        }
    }

}
