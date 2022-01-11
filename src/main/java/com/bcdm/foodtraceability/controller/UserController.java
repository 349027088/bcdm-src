package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.entity.UserInfo;
import com.bcdm.foodtraceability.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@RestController
@RequestMapping("/user")
public class UserController {

    @Autowired
    private UserService userService;

    @RequestMapping("/login")
    public UserInfo login(User user){

        UserInfo userInfo = userService.login(user);
        return userInfo;
    }

    @RequestMapping("/register")
    public UserInfo register(User user,UserInfo userInfo){

        UserInfo newUserInfo = userService.register(user,userInfo);
        return newUserInfo;
    }

    @RequestMapping("/modifyPassword")
    public UserInfo modifyPassword(User user){

        UserInfo userInfo = userService.modifyPassword(user);
        return userInfo;
    }

    @RequestMapping("/modifyUserInfo")
    public UserInfo modifyUserInfo(UserInfo userInfo){

        UserInfo newUserInfo = userService.modifyUserInfo(userInfo);
        return newUserInfo;
    }


}

