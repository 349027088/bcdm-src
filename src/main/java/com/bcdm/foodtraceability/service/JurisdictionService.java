package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.User;

import java.util.List;

/**
 * <p>
 *  关联信息服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface JurisdictionService extends IService<Jurisdiction> {

    /**
     * 获取用户的企业关联信息
     *
     * @param user 获取信息的用户
     * @return 返回该用户相关联的企业
     */
    List<Jurisdiction> getJurisdictionByUser(User user) throws Exception;

    /**
     * 获取企业的用户关联信息
     * @param company 获取信息的企业
     * @return 返回该用户相关联的用户
     */
    List<Jurisdiction> getJurisdictionByCompany(Company company) throws Exception;

    /**
     * 创建一个企业和用户的关联信息
     * @param user 需要关联的用户信息
     * @param company 需要关联的企业信息
     * @return 创建成功的关联信息
     * @throws Exception 创建新的关联信息失败
     */
    Jurisdiction createJurisdiction(User user,Company company,Integer identity) throws Exception;

}
