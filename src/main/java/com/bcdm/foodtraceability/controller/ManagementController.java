package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.ManagementService;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import org.springframework.stereotype.Controller;

import javax.validation.groups.Default;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/management")
public class ManagementController {

    @Autowired
    private ManagementService managementService;

    /**
     * 管理员登录Controller
     * @param management 登录用账号密码
     * @return 登录成功的用户信息
     * @throws Exception 登录失败
     */
    @PostMapping("/login")
    @CrossOrigin
    public ReturnItem<Management> login(@Validated @RequestBody Management management) throws Exception{
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.login(management));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("登录成功");
        return returnItem;
    }

    /**
     * 管理员注册Controller
     * @param management 需要注册的用户信息
     * @return 注册成功用户信息
     * @throws Exception 注册失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Management> register(@Validated({RegisterGroup.class, Default.class}) @RequestBody Management management) throws Exception{
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.register(management));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("注册成功");
        return returnItem;
    }

    /**
     * 管理员修改密码Controller
     * @param passwordInfo 用户账号密码和新密码
     * @return 修改成功用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modifyPassword")
    @CrossOrigin
    public ReturnItem<Management> modifyPassword(@Validated() @RequestBody String passwordInfo) throws Exception{
        JSONObject jsonObject = JSONObject.parseObject(passwordInfo);
        String newPassword = jsonObject.getString("newPassword");
        Management management = jsonObject.getObject("Management",Management.class);
        ReturnItem<Management> returnItem = new ReturnItem<>();
        returnItem.setT(managementService.modifyPassword(management,newPassword));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("密码修改成功");
        return returnItem;
    }

}

