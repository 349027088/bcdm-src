package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.*;
import com.baomidou.mybatisplus.extension.service.IService;

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
     * @throws Exception 新规企业授权失败
     */
    void createCompanyServiceEmpower(Company company) throws Exception;

    /**
     * 给企业授权
     *
     * @param empower 授权信息
     * @return 返回授权成功的信息
     * @throws Exception 企业授权失败
     */
    Empower createNewEmpower(Empower empower) throws Exception;

    /**
     * 修改企业授权信息
     *
     * @param empower 修改后的授权信息
     * @return 修改成功的授权信息
     */
    Empower modifyEmpower(Empower empower) throws Exception;

    /**
     * 查询所有授权信息
     *
     * @param selectInfo 查询信息的管理员
     * @return 查询所有授权信息
     */
    IPage<Empower> getEmpowerList(SelectPageEntity<Empower> selectInfo) throws Exception;


}
