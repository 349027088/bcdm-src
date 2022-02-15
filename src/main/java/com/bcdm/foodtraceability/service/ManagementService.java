package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Management;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.ModifyPassword;
import com.bcdm.foodtraceability.entity.User;

/**
 * <p>
 *  管理员服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface ManagementService extends IService<Management> {

    /**
     * 用于管理员登录
     *
     * @return 管理员信息
     */
    Management login(Management management) throws Exception;

    /**
     * 用于管理员注册
     *
     * @param management 管理员登录信息
     * @return 管理员信息
     */
    Management register(Management management) throws Exception;

    /**
     * 修改密码
     *
     * @param userLoginInfo 修改用登录信息
     * @return 管理员信息
     */
    Management modifyPassword(ModifyPassword userLoginInfo) throws Exception;

    /**
     * 修改管理员信息
     *
     * @param management 修改用管理员信息
     * @return 管理员信息
     */
    Management modifyUserInfo(Management management) throws Exception;

    /**
     * 管理员判定
     *
     * @param managementId 用于判定是不是管理员得ID
     * @return 管理员不存在
     */
    Boolean checkManager(String managementId) throws Exception;
}
