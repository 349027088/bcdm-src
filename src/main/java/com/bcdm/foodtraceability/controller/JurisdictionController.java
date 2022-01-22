package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.COMPANY_ICON_MODIFY_SUCCESS;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/jurisdiction")
public class JurisdictionController {

    private final JurisdictionService jurisdictionService;


    public JurisdictionController(JurisdictionService jurisdictionService) {
        this.jurisdictionService = jurisdictionService;
    }

    /**
     * 修改关联用户权限
     *
     * @param jsonInfo 需要修改的关联信息和修改权限的管理
     * @return 返回修改图片的地址
     * @throws Exception 图片修改失败
     */
    @PostMapping("modify")
    @CrossOrigin
    public ReturnItem<Boolean> modifyCompanyIcon(@RequestPart String jsonInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(jsonInfo);
        Jurisdiction jurisdiction = jsonObject.getObject("jurisdiction",Jurisdiction.class);
        Integer companyManagerUserId = jsonObject.getInteger("userId");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(jurisdictionService.modifyJurisdiction(jurisdiction,companyManagerUserId));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_ICON_MODIFY_SUCCESS);
        return returnItem;
    }
}

