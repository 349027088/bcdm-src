package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 关联信息服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface JurisdictionService extends IService<Jurisdiction> {

    /**
     * 获取用户的企业关联信息
     *
     * @param userId 获取信息的用户
     * @return 返回该用户相关联的企业
     */
    Jurisdiction getJurisdictionByUser(Integer userId) throws Exception;

    /**
     * 获取企业的用户关联信息
     *
     * @param CompanyId 获取信息的企业
     * @return 返回该用户相关联的用户
     */
    List<Jurisdiction> getJurisdictionByCompany(Integer CompanyId) throws Exception;

    /**
     * 创建一个企业和用户的关联信息
     *
     * @param userId    需要关联的用户信息
     * @param companyId 需要关联的企业信息
     * @param identity  用户被授予的职位
     * @throws Exception 创建新的关联信息失败
     */
    void createJurisdiction(Integer userId, Integer companyId, Integer identity) throws Exception;

    /**
     * 修改用户在公司的职位
     *
     * @param jurisdiction   需要修改职位的员工和公司信息以及新的职位
     * @param companyManagerUserId 公司管理员ID
     * @return 修改信息结果
     * @throws Exception 修改关联信息失败
     */
    Boolean modifyJurisdiction(Jurisdiction jurisdiction, Integer companyManagerUserId) throws Exception;

    /**
     * 删除用户在公司的职位
     *
     * @param jurisdiction   删除对应的关联信息
     * @param companyManagerUserId 公司管理员ID
     * @return 删除信息结果
     * @throws Exception 删除关联信息失败
     */
    Boolean deleteJurisdiction(Jurisdiction jurisdiction, Integer companyManagerUserId) throws Exception;

    /**
     * 修改通知信息获取的最新时间
     *
     * @param jurisdiction 需要修改的信息
     * @return 修改信息结果
     * @throws Exception 修改通知信息失败
     */
    Boolean modifyNoticeCheck(Jurisdiction jurisdiction) throws Exception;
}
