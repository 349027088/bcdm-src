package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.entity.ModifyPassword;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.ManagementService;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.groups.Default;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/management")
public class ManagementController {

    private final ManagementService managementService;

    public ManagementController(ManagementService managementService) {
        this.managementService = managementService;
    }

    /**
     * 管理员登录Controller
     *
     * @param management 登录用账号密码
     * @return 登录成功的用户信息
     * @throws Exception 登录失败
     */
    @PostMapping("/login")
    @CrossOrigin
    public ReturnItem<Management> login(@Validated({GetInfoGroup.class})
                                            @RequestBody Management management) throws Exception {
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.login(management));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(LOGIN_SUCCESS);
        return returnItem;
    }

    /**
     * 管理员注册Controller
     *
     * @param management 需要注册的用户信息
     * @return 注册成功用户信息
     * @throws Exception 注册失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Management> register(@Validated({RegisterGroup.class})
                                               @RequestBody Management management) throws Exception {
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.register(management));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(REGISTER_SUCCESS);
        return returnItem;
    }

    /**
     * 管理员修改信息Controller
     *
     * @param management 用户账号密码和新密码
     * @return 修改成功的用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Management> modify(@Validated({ModifyGroup.class})
                                         @RequestBody Management management) throws Exception {
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.modifyUserInfo(management));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
        return returnItem;
    }

    /**
     * 管理员修改密码Controller
     *
     * @param userLoginInfo 用户账号密码和新密码
     * @return 修改成功用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modifyPassword")
    @CrossOrigin
    public ReturnItem<Management> modifyPassword(@Validated({ModifyGroup.class})
                                                     @RequestBody ModifyPassword userLoginInfo) throws Exception {
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.modifyPassword(userLoginInfo));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
        return returnItem;
    }

//    /**
//     * 管理员修改密码Controller
//     *
//     * @param passwordInfo 用户账号密码和新密码
//     * @return 修改成功用户信息
//     * @throws Exception 修改失败
//     */
//    @PostMapping("/delete")
//    @CrossOrigin
//    public ReturnItem<Management> delete(@Validated() @RequestBody String passwordInfo) throws Exception {
//        JSONObject jsonObject = JSONObject.parseObject(passwordInfo);
//        String newPassword = jsonObject.getString("newPassword");
//        Management management = jsonObject.getObject("management", Management.class);
//        ReturnItem<Management> returnItem = new ReturnItem<>();
//        returnItem.setT(managementService.modifyPassword(management, newPassword));
//        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
//        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
//        return returnItem;
//    }

}

