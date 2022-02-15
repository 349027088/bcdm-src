package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Company;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.entity.User;

import java.util.Map;

/**
 * <p>
 * 企业服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface CompanyService extends IService<Company> {

    /**
     * 管理员获取指定企业
     *
     * @param selectPageEntity 查询条件
     * @return 指定的企业信息
     */
    IPage<Company> getCompanyList(SelectPageEntity<Company> selectPageEntity) throws Exception;

    /**
     * 创建企业信息
     *
     * @param company 需要创建的企业信息
     * @return 创建成功的企业信息
     */
    Company register(Company company) throws Exception;


    /**
     * 修改企业信息
     *
     * @param company 需要修改信息的企业
     * @return 修改成功的企业信息
     */
    Company modify(Company company) throws Exception;

    /**
     * 获取企业信息
     *
     * @param company 需要获取的企业编号
     * @return 对应的企业信息
     */
    Company getCompanyInfo(Company company) throws Exception;

    /**
     * 获取用户的企业信息
     *
     * @param user 需要查询名下企业的用户
     * @return 该用户名下的企业信息
     */
    Map<String, Object> getCompanyByUser(User user) throws Exception;

    /**
     * 登录员工信息到企业
     *
     * @param company 需要被登录到的企业信息和员工的信息
     * @return 被登录的企业信息
     * @throws Exception 登录员工信息失败
     */
    Company createUserToCompany(Company company) throws Exception;
    /**
     * 修改企业营业执照信息
     *
     * @param company 需要修改信息的企业
     * @return 修改成功的企业信息
     */
    Boolean modifyBusinessLicense(String managementId, Company company) throws Exception;

    /**
     * 修改企业经营许可证信息
     *
     * @param company 需要修改信息的企业
     * @return 修改成功的企业信息
     */
    Boolean modifyHealthPermit(String managementId, Company company) throws Exception;

}
