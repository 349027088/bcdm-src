package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.User;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.UserInfo;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
public interface UserService extends IService<User> {
    /**
     * 用于用户登录
     * @return 用户信息
     */
    UserInfo login(User user);

    /**
     * 用于用户注册
     * @param user 用户登录信息
     * @param userInfo 用户信息
     * @return 用户信息
     */
    UserInfo register(User user,UserInfo userInfo);

    /**
     * 修改密码
     * @param user 修改用登录信息
     * @return 用户信息
     */
    UserInfo modifyPassword(User user);

    /**
     * 修改用户信息
     * @param userInfo 修改用用户信息
     * @return 用户信息
     */
    UserInfo modifyUserInfo(UserInfo userInfo);
}
