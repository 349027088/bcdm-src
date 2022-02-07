package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.*;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Map;

/**
 * <p>
 * 用户服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface UserService extends IService<User> {

    /**
     * 用于用户登录
     *
     * @return 用户信息
     */
    Map<String, Object> login(User user) throws Exception;

    /**
     * 用于用户注册
     *
     * @param user 用户登录信息
     * @return 用户信息
     */
    User register(User user) throws Exception;

    /**
     * 修改密码
     *
     * @param userLoginInfo 修改用登录信息
     * @return 用户信息
     */
    User modifyPassword(ModifyPassword userLoginInfo) throws Exception;

    /**
     * 修改用户信息
     *
     * @param user 修改用用户信息
     * @return 用户信息
     */
    User modifyUserInfo(User user) throws Exception;

    /**
     * 锁定用户
     *
     * @param user 锁定用户
     * @return 成功与否
     */
    int lockUser(User user) throws Exception;

    /**
     * 解锁用户
     *
     * @param user 解锁用户
     * @return 成功与否
     */
    int unLockUser(User user) throws Exception;

    /**
     * 获取企业的所有员工
     *
     * @param selectInfo 需要获取员工的企业
     * @return 取得的员工列表
     */
    IPage<UserModel> getUserByCompany(SelectPageEntity<UserModel> selectInfo) throws Exception;

}
