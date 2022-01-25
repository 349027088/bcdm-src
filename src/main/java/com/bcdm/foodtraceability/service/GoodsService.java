package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Goods;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.GoodsType;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * <p>
 * 商品服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface GoodsService extends IService<Goods> {

    /**
     * 获取公司的商品列表
     *
     * @param companyId 需要获取商品列表的公司ID
     * @return 获取的商品列表
     * @throws Exception 返回商品列表失败
     */
    List<Goods> getGoodsListByCompany(Integer companyId) throws Exception;

    /**
     * 查询待审核或者审核失败的的商品列表
     * @param status 需要查询商品的状态
     * @return 被查询的商品信息
     * @throws Exception 查询信息出现错误
     */
    List<Goods> getGoodsListByStatus(Integer status) throws Exception;

    /**
     * 按照种类查询商品信息
     * @param companyId 需要获取商品列表的公司ID
     * @param goodsTypeId 被查询的商品种类ID
     * @return 被查询的商品信息
     * @throws Exception 查询信息出现错误
     */
    List<Goods> getGoodsListByGoodsType(Integer companyId, Integer goodsTypeId) throws Exception;

    /**
     * 公司添加一个新的商品
     *
     * @param goods   新建商品的信息
     * @return 创建成功的商品
     * @throws Exception 创建商品失败
     */
    Boolean createGoods(Goods goods) throws Exception;

    /**
     * 批量创建商品
     *
     * @param company   创建商品的公司
     * @param goodsList 批量创建的商品信息
     * @return 创建成功的商品列表
     * @throws Exception 创建商品失败
     */
    List<Goods> createGoodsList(Company company, List<Goods> goodsList) throws Exception;

    /**
     * 批量修改商品信息
     *
     * @param company   修改商品信息的公司
     * @param goodsList 批量修改的商品信息
     * @return 修改成功的商品列表
     * @throws Exception 修改失败的商品列表
     */
    List<Goods> modifyGoodsList(Company company, List<Goods> goodsList) throws Exception;

    /**
     * 需要修改的商品信息
     *
     * @param company 修改商品信息的公司
     * @param goods 需要修改的商品信息
     * @param icon 需要上传的商品图片
     * @return 修改成功的商品信息
     * @throws Exception 修改商品信息失败
     */
    Goods modifyGoods(Company company, Goods goods,MultipartFile icon) throws Exception;

    /**
     * 删除一个商品信息
     * @param company 需要删除商品信息的公司
     * @param goods 需要删除的商品信息
     * @return 删除是否成功
     * @throws Exception 删除信息失败
     */
    boolean deleteGoods(Company company,Goods goods) throws Exception;

    /**
     * 批量删除商品信息
     * @param company 需要删除商品信息的公司
     * @param goodsList 需要删除的商品列表
     * @return 删除成功条数
     * @throws Exception 删除信息失败
     */
    int deleteGoodsList(Company company,List<Goods> goodsList)throws Exception;


}
