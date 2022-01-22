package com.bcdm.foodtraceability.controller;

import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.UserService;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.groups.Default;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 用户前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/user")
public class UserController {

    private final UserService userService;

    private final CompanyService companyService;

    public UserController(UserService userService, CompanyService companyService) {
        this.userService = userService;
        this.companyService = companyService;
    }

    /**
     * 用户登录Controller
     *
     * @param user 登录用账号密码
     * @return 登录成功的用户信息
     * @throws Exception 登录失败
     */
    @PostMapping("/login")
    @CrossOrigin
    public ReturnItem<User> login(@Validated @RequestBody User user) throws Exception {
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.login(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(LOGIN_SUCCESS);
        return returnItem;
    }

    /**
     * 用户注册Controller
     *
     * @param user 需要注册的用户信息
     * @return 注册成功用户信息
     * @throws Exception 注册失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<User> register(@Validated({RegisterGroup.class, Default.class}) @RequestBody User user) throws Exception {
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.register(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(REGISTER_SUCCESS);
        return returnItem;
    }

    /**
     * 修改密码Controller
     *
     * @param passwordInfo 用户账号密码和新密码
     * @return 修改成功用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modifyPassword")
    @CrossOrigin
    public ReturnItem<User> modifyPassword(@Validated @RequestBody String passwordInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(passwordInfo);
        String newPassword = jsonObject.getString("newPassword");
        User user = jsonObject.getObject("user", User.class);
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.modifyPassword(user, newPassword));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
        return returnItem;
    }

    /**
     * 获得用户的所有公司信息
     *
     * @param user 获取公司信息的用户
     * @return 该用户所属公司信息
     * @throws Exception 查询公司信息失败
     */
    @PostMapping("/getCompanyByUser")
    @CrossOrigin
    public ReturnItem<List<Company>> getCompanyByUser(@RequestBody User user) throws Exception {
        ReturnItem<List<Company>> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.getCompanyByUser(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业信息获取成功");
        return returnItem;
    }

}

