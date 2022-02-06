package com.bcdm.foodtraceability.controller;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ModifyPassword;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.UserService;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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

    public UserController(UserService userService) {
        this.userService = userService;
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
    public ReturnItem<User> register(@Validated({RegisterGroup.class})
                                     @RequestBody User user) throws Exception {
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.register(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(REGISTER_SUCCESS);
        return returnItem;
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
    public ReturnItem<Company> login(@Validated({GetInfoGroup.class})
                                  @RequestBody User user) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(userService.login(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(LOGIN_SUCCESS);
        return returnItem;
    }

    /**
     * 修改用户信息Controller
     *
     * @param user 用户账号密码和新密码
     * @return 修改成功的用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<User> modify(@Validated({ModifyGroup.class})
                                   @RequestBody User user) throws Exception {
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.modifyUserInfo(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
        return returnItem;
    }

    /**
     * 修改密码Controller
     *
     * @param userLoginInfo 用户账号密码和新密码
     * @return 修改成功用户信息
     * @throws Exception 修改失败
     */
    @PostMapping("/modifyPassword")
    @CrossOrigin
    public ReturnItem<User> modifyPassword(@Validated({ModifyGroup.class})
                                           @RequestBody ModifyPassword userLoginInfo) throws Exception {
        ReturnItem<User> returnItem = new ReturnItem<>();
        returnItem.setT(userService.modifyPassword(userLoginInfo));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_PASSWORD_SUCCESS);
        return returnItem;
    }

}

