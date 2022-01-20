package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Empower;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.Management;

import java.util.List;

/**
 * <p>
 * 授权信息服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface EmpowerService extends IService<Empower> {

    /**
     * 新规企业自动授权服务
     *
     * @param company 新规企业信息
     * @return 创建成功的授权信息
     * @throws Exception 新规企业授权失败
     */
    Empower createCompanyServiceEmpower(Company company) throws Exception;

    /**
     * 给企业授权
     *
     * @param management 创建新授权信息的管理员
     * @param empower    授权信息
     * @return 返回授权成功的信息
     * @throws Exception 企业授权失败
     */
    Empower createNewEmpower(Management management, Empower empower) throws Exception;

    /**
     * 修改企业授权信息
     *
     * @param management 修改企业授权信息的管理员
     * @param empower    修改后的授权信息
     * @return 修改成功的授权信息
     */
    Empower modifyEmpower(Management management, Empower empower) throws Exception;

    /**
     * 查询所有授权信息
     *
     * @param management 查询信息的管理员
     * @return 查询所有授权信息
     */
    List<Empower> getEmpowerList(Management management) throws Exception;


}
