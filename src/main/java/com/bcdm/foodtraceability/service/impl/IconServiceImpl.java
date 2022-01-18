package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.service.IconService;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import static com.bcdm.foodtraceability.common.Constants.JPG_TYPE_ICON;
import static com.bcdm.foodtraceability.common.Constants.PNG_TYPE_ICON;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.ICON_SIZE_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.ICON_TYPE_FORMAT_FAIL;

@Service
public class IconServiceImpl implements IconService {

    @Override
    public String createIcon(MultipartFile icon) throws Exception {
        //TODO
        return null;
    }

    @Override
    public void deleteIcon(String URI) throws Exception {
        //TODO
    }

    @Override
    public boolean IconCheck(MultipartFile icon) throws Exception {
        String originalFilename = icon.getOriginalFilename();
        String suffixName = originalFilename.substring(originalFilename.lastIndexOf("."));
        if (!(JPG_TYPE_ICON.equals(suffixName)||PNG_TYPE_ICON.equals(suffixName))){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,ICON_TYPE_FORMAT_FAIL);
        }
        if (!iconCheckSize(icon)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,ICON_SIZE_FAIL);
        }
        return false;
    }

    @Override
    public String sendIconToCloud(MultipartFile icon) throws Exception {
        //TODO
        return null;
    }

    private boolean iconCheckSize(MultipartFile icon){
        //TODO
        return false;
    }


}
