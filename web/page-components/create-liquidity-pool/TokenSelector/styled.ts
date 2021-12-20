import styled from 'styled-components';
import Image from '../../../components/Image';

export const TokenWrapper = styled.div`
  min-width: 220px;
  padding: 0 10px;
`;

export const SelectTokenButton = styled.div`
  -webkit-box-align: center;
  display: flex;
  align-items: center;
  font-weight: 600;
  background-color: transparent;
  color: rgb(255, 255, 255);
  border-radius: 12px;
  /*box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;*/
  cursor: pointer;
  user-select: none;
  border: none;
  padding: 10px 20px;
  font-size: 20px;
  margin: 0;
  white-space: nowrap;
  outline: none !important;
  text-align: left;
`;

export const TokenImage = styled(Image)`
  margin-right: 12px;
  width: 44px;
  height: 44px;
  border-radius: 50%;
  box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;
`;

export const TokenDetails = styled.div`
  margin-left: 10px;
`;

export const TokenAction = styled.span`
  box-sizing: border-box;
  margin: 0;
  min-width: 0;
  font-weight: 500;
  font-size: 14px;
  color: rgb(86, 90, 105);
  display: block;
`;

export const TokenSymbol = styled.span`
  transition: all 200ms;
  background-image: -webkit-gradient(
    linear,
    left top,
    right bottom,
    color-stop(0, #f80179),
    color-stop(1, #bb0691)
  );
  background-image: gradient(
    linear,
    left top,
    right bottom,
    color-stop(0, #f80179),
    color-stop(1, #bb0691)
  );
  color: transparent;
  -webkit-background-clip: text;
  background-clip: text;
`;

export const TokenDropdown = styled.ul<{ isActive: boolean }>`
  display: none;
  position: absolute;
  list-style: none;
  left: 30px;
  border-radius: 8px;
  background: #14142b;
  z-index: 22;
  padding: 10px 0;
  width: 200px;
  box-shadow: 0 8px 24px rgba(78, 75, 102, 0.2);

  ${({ isActive }) => isActive && 'display: block;'}
`;

export const TokenDropdownItem = styled.li<{ isDisabled: boolean }>`
  display: flex;
  align-items: center;
  padding: 8px 20px;
  cursor: pointer;

  &:hover {
    background-color: #262338;
  }

  ${({ isDisabled }) =>
    isDisabled &&
    `
  pointer-events: none;
  opacity: 0.2;
  `}
`;

export const TokenItemImage = styled(Image)`
  width: 24px;
  height: 24px;
  box-shadow: rgba(0, 0, 0, 0.08) 0 6px 10px;
  border-radius: 24px;
  margin-right: 20px;
`;

export const TokenItemSymbol = styled.span`
  font-size: 16px;
  line-height: 16px;
  margin: 0;
  font-weight: 500;
  display: block;
  color: #f7f7f7;
`;

export const TokenItemName = styled.span`
  margin: 0;
  font-weight: 300;
  font-size: 12px;
  display: block;
  color: #f7f7f7;
`;
