package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.MODIFY_NOTICE_CHECK_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.MODIFY_USER_TO_COMPANY_SUCCESS;

/**
 * <p>
 * 企业用户关联前端控制器
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
     * @return 修改关联信息成功状态
     * @throws Exception 关联信息修改失败
     */
    @PostMapping("modify")
    @CrossOrigin
    public ReturnItem<Boolean> modifyJurisdiction(@RequestBody String jsonInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(jsonInfo);
        Jurisdiction jurisdiction = jsonObject.getObject("jurisdiction", Jurisdiction.class);
        Integer companyManagerUserId = jsonObject.getInteger("userId");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(jurisdictionService.modifyJurisdiction(jurisdiction, companyManagerUserId));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_USER_TO_COMPANY_SUCCESS);
        return returnItem;
    }

    /**
     * 修改用户的通知
     *
     * @param jurisdiction 需要修改的关联信息和修改权限的管理
     * @return 修改关联信息成功状态
     * @throws Exception 关联信息修改失败
     */
    @PostMapping("changeNotice")
    @CrossOrigin
    public ReturnItem<Boolean> changeNotice(@RequestBody Jurisdiction jurisdiction) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(jurisdictionService.modifyNoticeCheck(jurisdiction));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_NOTICE_CHECK_SUCCESS);
        return returnItem;
    }

    /**
     * 删除关联用户权限
     *
     * @param jsonInfo 需要刪除的員工关联信息
     * @return 删除关联信息成功状态
     * @throws Exception 删除信息失败
     */
    @PostMapping("delete")
    @CrossOrigin
    public ReturnItem<Boolean> deleteJurisdiction(@RequestBody String jsonInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(jsonInfo);
        Jurisdiction jurisdiction = jsonObject.getObject("jurisdiction", Jurisdiction.class);
        Integer companyManagerUserId = jsonObject.getInteger("userId");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(jurisdictionService.deleteJurisdiction(jurisdiction, companyManagerUserId));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_USER_TO_COMPANY_SUCCESS);
        return returnItem;
    }
}

