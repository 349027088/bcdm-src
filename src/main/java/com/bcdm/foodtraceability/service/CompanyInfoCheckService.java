package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.CompanyInfoCheck;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author 王
 * @since 2022-02-14
 */
public interface CompanyInfoCheckService extends IService<CompanyInfoCheck> {


    /**
     * 上传新的营业执照信息
     *
     * @param company 需要修改信息的企业
     * @return 上传结果
     */
    Boolean modifyBusinessLicense(CompanyInfoCheck company) throws Exception;

    /**
     * 上传新的经营许可证信息
     *
     * @param company 需要修改信息的企业
     * @return 上传结果
     */
    Boolean modifyHealthPermit(CompanyInfoCheck company) throws Exception;

    /**
     * 删除中间经营信息表
     *
     * @param company 需要删除中间表信息的企业
     * @throws Exception 删除失败
     */
    void deleteCheckInfo(Company company) throws Exception;

}
