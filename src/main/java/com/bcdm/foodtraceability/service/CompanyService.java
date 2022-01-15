package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.User;

import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface CompanyService extends IService<Company> {

    /**
     * 创建企业信息
     *
     * @param company 需要创建的企业信息
     * @return 创建成功的企业信息
     */
    Company register(User user, Company company) throws Exception;


    /**
     * 修改企业信息
     *
     * @param user 操作的用户
     * @param company 需要修改信息的企业
     * @return 修改成功的企业信息
     */
    Company modify(User user, Company company) throws Exception;

    /**
     * 获取用户的企业信息
     *
     * @param user 需要查询名下企业的用户
     * @return 该用户名下的企业信息
     */
    List<Company> getCompanyInfo(User user) throws Exception;
}
