package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ModifyPassword;
import com.bcdm.foodtraceability.entity.User;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.UserModel;

import java.util.List;

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
    User login(User user) throws Exception;

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
     * @param company 需要获取员工的企业
     * @return 取得的员工列表
     */
    List<UserModel> getUserByCompany(Company company) throws Exception;

}
