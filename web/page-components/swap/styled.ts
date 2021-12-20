import styled from 'styled-components';

export const SwapBlockHeader = styled.div`
  display: flex;
  align-items: center;
  color: rgb(0, 0, 0);
  font-size: 0.75rem;
  line-height: 1rem;
  padding: 0.75rem 1rem 0;

  span {
    box-sizing: border-box;
    margin: 0;
    min-width: 0;
    font-weight: 500;
    font-size: 14px;
    color: rgb(86, 90, 105);
  }
`;

export const RotateWrapper = styled.div`
  padding: 1rem;
  opacity: 0.8;
  display: block;
  text-align: center;
`;

export const RotateButton = styled.button`
  cursor: pointer;
  background: transparent;
  outline: none !important;
  border: none;
`;

export const DialogFooter = styled.div`
  padding: 1rem 0 2rem 0;
`;
