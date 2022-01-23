package com.bcdm.foodtraceability.controller;

import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.IconService;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.ICON_UPLOAD_SUCCESS;

/**
 * <p>
 * 图片前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/icon")
public class IconController {

    private final IconService iconService;

    public IconController(IconService iconService) {
        this.iconService = iconService;
    }

    /**
     * 创建图片
     *
     * @param file 需要被创建的企业头像图片
     * @return 创建成功的企业头像地址
     * @throws Exception 企业头像创建失败
     */
    @PostMapping("createIcon")
    @CrossOrigin
    public ReturnItem<String> createIcon(@RequestPart MultipartFile file) throws Exception {
        ReturnItem<String> returnItem = new ReturnItem<>();
        returnItem.setT(iconService.createIcon(file));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ICON_UPLOAD_SUCCESS);
        return returnItem;
    }
}
